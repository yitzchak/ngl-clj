import {
  WidgetModel,
  WidgetView,
} from '@jupyter-widgets/base';

// eslint-disable-next-line @typescript-eslint/no-var-requires
//const widgets = require('@jupyter-widgets/base');

// eslint-disable-next-line @typescript-eslint/no-var-requires
const NGL = require('ngl');

import { MODULE_NAME, MODULE_VERSION } from './version';

import { serialize_model } from './utils';

export class TrajectoryModel extends WidgetModel {
  defaults() {
    return {
      ...super.defaults(),

      direction: 'forward',
      ext: null,
      frame: 0,
      count: null,
      interpolate_step: 5,
      interpolate_type: "",
      is_running: false,
      mode: 'loop',
      name: '',
      step: 1,
      timeout: 50,
      value: null,
      start: 0,
      end: null,

      _model_name: 'TrajectoryModel',
      _model_module: MODULE_NAME,
      _model_module_version: MODULE_VERSION,
      _view_name: 'TrajectoryView',
      _view_module: MODULE_NAME,
      _view_module_version: MODULE_VERSION,
    };
  }
}

export class TrajectoryView extends WidgetView {
  stage_obj: any;
  component_obj: any;
  trajectory_obj: any;
  rendered = false;
  frame_set_time: any;
  count_callbacks: Function[] = [];
  frame_callbacks: Function[] = [];

  initialize(parameters: any): void {
    super.initialize(parameters);
    this.stage_obj = this.options.stage_obj;
    this.component_obj = this.options.component_obj;
    this.model.on('msg:custom', this.handle_custom_message.bind(this));
    this.model.on('change:is_running', this.change_is_running.bind(this));
    this.model.on('change:frame', this.change_frame.bind(this));
    this.model.on_some_change([
      'step',
      'timeout',
      'interpolate_type',
      'interpolate_step',
      'start',
      'end',
      'mode',
      'direction',
    ], this.update_player_parameters, this);
  }

  change_is_running(event: any): void {
    if (this.trajectory_obj) {
      if (event.changed.is_running) {
        if (!this.trajectory_obj.trajectory.player.isRunning) {
          this.trajectory_obj.trajectory.player.play();
        }
      } else {
        if (this.trajectory_obj.trajectory.player.isRunning) {
          this.trajectory_obj.trajectory.player.pause();
        }
      }
    }
  }

  change_frame(event: any): void {
    if (this.trajectory_obj && this.trajectory_obj.trajectory.frame !== event.changed.frame) {
      this.trajectory_obj.trajectory.setFrame(event.changed.frame);
    }
  }

  update_player_parameters(): void {
    if (this.trajectory_obj) {
      const params = serialize_model(this.model,
                                     { 'step': null, 'timeout': null,
                                       'interpolateType': null,
                                       'interpolateStep': null,
                                       'mode': null,
                                       'start': null,
                                       'end': null,
                                       'direction': null });
      this.trajectory_obj.trajectory.player.setParameters(params);
    }
  }

  handle_custom_message(content: any, buffers: DataView[]): void {
    var cb;

    if (this.trajectory_obj) {
      switch (content.do) {
        case 'play':
          this.trajectory_obj.trajectory.player.play();
          break;
        case 'pause':
          this.trajectory_obj.trajectory.player.pause();
          break;
        case 'stop':
          this.trajectory_obj.trajectory.player.stop();
          break;
        case 'count':
          cb = this.count_callbacks.pop();
          if (cb) cb(content.count);
          break;
        case 'frame':
          cb = this.frame_callbacks.pop();
          if (cb) cb(content.i, content.box, new Float32Array(buffers[0].buffer), content.count);
          break;
      }
    }
  }

  get_parameters(): any {
    return serialize_model(this.model, { 'name': null });
  }

  wire_view(): void {
    this.update_player_parameters();

    if (this.trajectory_obj.name != this.model.get('name')) {
      this.model.set('name', this.trajectory_obj.name);
      this.model.save_changes();
    }

    this.trajectory_obj.signals.nameChanged.add((name: string): void => {
      this.model.set('name', name);
      this.model.save_changes();
    });

    this.trajectory_obj.signals.frameChanged.add((frame: number): void => {
      if (frame != this.model.get('frame')) {
        this.model.set('frame', frame);
        let now = Date.now();
        if (!this.model.get('is_running') || !this.frame_set_time ||
            (now - this.frame_set_time) >= 1000) {
          this.frame_set_time = now;
          this.model.save_changes();
        }
      }
    });

    this.trajectory_obj.signals.countChanged.add((count: number): void => {
      this.model.set('count', count);
      this.model.save_changes();
    });

    this.trajectory_obj.trajectory.player.signals.startedRunning.add((): void => {
      this.model.set('is_running', true);
      this.model.save_changes();
    });

    this.trajectory_obj.trajectory.player.signals.haltedRunning.add((): void => {
      this.model.set('is_running', false);
      this.model.save_changes();
    });
  }

  async load_file(): Promise<any> {
    var value: any = this.model.get('value');
    var ext: any = this.model.get('ext');
    let params: any = {};

    if (value && ext) {
      params.ext = ext;
    	value = new Blob([(value instanceof DataView) ? value.buffer : value],
    	                 { type: (typeof value === 'string' || value instanceof String)
    	                            ? 'text/plain'
    	                            : 'application/octet-binary' });
    }

    if (value === null) { // Trajectory associated with the structure
      this.trajectory_obj = await this.component_obj.addTrajectory('', this.get_parameters());
    } else if (typeof value === 'string' && value.startsWith('jupyter:')) { // Jupyter remote trajectory
      this.trajectory_obj = await this.component_obj.addTrajectory(this.request.bind(this), this.get_parameters());
    } else { // Trajectory passed by value or normal NGL remote trajectory
      this.trajectory_obj = await this.component_obj.addTrajectory(await NGL.autoLoad(value, params),
                                                                   this.get_parameters());
    }
  }

  request(callback: Function, i?: number, atom_indices?: number[][]): void {
    if (typeof i === 'number') {
      this.frame_callbacks.push(callback);
      this.send({ event: 'frame', i, atom_indices }, []);
    } else {
      this.count_callbacks.push(callback);
      this.send({ event: 'count' }, []);
    }
  }

  async render() {
    super.render();
    if (this.component_obj && !this.trajectory_obj && !this.rendered) {
      this.rendered = true;
      await this.load_file();
  	  this.wire_view();
    }
  }

  remove() {
    super.remove();
    if (this.component_obj && this.trajectory_obj) {
      this.component_obj.removeTrajectory(this.trajectory_obj);
    }
    this.trajectory_obj = null;
  }
}

