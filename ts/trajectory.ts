import {
  ISerializers,
  WidgetModel,
  WidgetView,
} from '@jupyter-widgets/base';

// eslint-disable-next-line @typescript-eslint/no-var-requires
//const widgets = require('@jupyter-widgets/base');

import { MODULE_NAME, MODULE_VERSION } from './version';

import { serialize_model } from './utils';

export class TrajectoryModel extends WidgetModel {
  defaults() {
    return {
      ...super.defaults(),

      name: '',
      frame: 0,
      step: null,
      timeout: 50,
      interpolate_type: "",
      interpolate_step: 5,
      mode: 'loop',
      direction: 'forward',
      is_running: false,

      _model_name: 'TrajectoryModel',
      _model_module: MODULE_NAME,
      _model_module_version: MODULE_VERSION,
      _view_name: 'TrajectoryView',
      _view_module: MODULE_NAME,
      _view_module_version: MODULE_VERSION,
    };
  }

  static serializers: ISerializers = {
    ...WidgetModel.serializers,
  };
}

export class TrajectoryView extends WidgetView {
  component_obj: any;
  trajectory_obj: any;
  rendered = false;

  initialize(parameters: any): void {
    super.initialize(parameters);
    this.component_obj = this.options.component_obj;
    this.model.on('change:is_running',
                  (event: any) => {
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
                  });
    this.model.on_some_change([
      'step',
      'timeout',
      'interpolate_type',
      'interpolate_step',
      'mode',
      'direction',
    ], this.update_player_parameters, this);
  }

  update_player_parameters(): void {
    if (this.trajectory_obj) {
      const params = serialize_model(this.model,
                                     { 'step': null, 'timeout': null,
                                       'interpolateType': null,
                                       'interpolateStep': null,
                                       'mode': null,
                                       'direction': null });
      this.trajectory_obj.trajectory.player.setParameters(params);
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
  }

  async render() {
    super.render();
    if (this.component_obj && !this.trajectory_obj && !this.rendered) {
      this.rendered = true;
      var value: any = this.model.get('value');
      this.trajectory_obj = await this.component_obj.addTrajectory(value, this.get_parameters());
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

