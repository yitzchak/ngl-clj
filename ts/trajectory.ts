import {
  ISerializers,
  WidgetModel,
  WidgetView,
} from '@jupyter-widgets/base';

// eslint-disable-next-line @typescript-eslint/no-var-requires
//const widgets = require('@jupyter-widgets/base');

import { MODULE_NAME, MODULE_VERSION } from './version';

// eslint-disable-next-line @typescript-eslint/no-var-requires
const camelCase = require('camelcase');


export class TrajectoryModel extends WidgetModel {
  defaults() {
    return {
      ...super.defaults(),

      name: '',
      initial_frame: 0,
      default_step: null,
      default_timeout: 0,
      default_interpolate_type: "",
      default_interpolate_step: 50,
      default_mode: 'loop',
      default_direction: 'forward',

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
  }

  parameter_names(): Array<string> {
    return ['initial_frame', 'default_step',
                                           'default_timeout',
                                           'default_interpolate_type',
                                           'default_interpolate_step',
                                           'default_mode', 'default_direction'];
  }

  get_parameters(): any {
    var params: any = {};

    for (const name of this.parameter_names()) {
      var value: any = this.model.get(name);
      if (value != null) {
        params[camelCase(name)] = value;
      }
    }

    return params;
  }

  wire_view(): void {
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
  	  //console.log(this.trajectory_obj.trajectory.player);
  	  this.trajectory_obj.trajectory.player.interpolateType = 'spline';
  	  this.trajectory_obj.trajectory.player.interpolateStep = 50;
  	  this.trajectory_obj.trajectory.player.play();
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

