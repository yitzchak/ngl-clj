import {
  ISerializers,
  WidgetModel,
  WidgetView,
} from '@jupyter-widgets/base';

// eslint-disable-next-line @typescript-eslint/no-var-requires
const widgets = require('@jupyter-widgets/base');

import { MODULE_NAME, MODULE_VERSION } from './version';

// eslint-disable-next-line @typescript-eslint/no-var-requires
const camelCase = require('camelcase');


export class ComponentModel extends WidgetModel {
  defaults() {
    return {
      ...super.defaults(),

      visible: true,
      name: null,
      positions: null,
      quaternion: [0.0, 0.0, 0.0, 0.0],
      scale: 1.0,
      representations: [],

      _model_module: MODULE_NAME,
      _model_module_version: MODULE_VERSION,
      _view_module: MODULE_NAME,
      _view_module_version: MODULE_VERSION,
    };
  }

  static serializers: ISerializers = {
    representations: { deserialize: widgets.unpack_models },
    ...WidgetModel.serializers,
  };
}

export class ComponentView extends WidgetView {
  stage_obj: any;
  component_obj: any;
  representationViews: any;
  in_representations_changing = false;
  rendered = false;

  initialize(parameters: any): void {
    super.initialize(parameters);
    this.stage_obj = this.options.stage_obj;

    this.representationViews = new widgets.ViewList(
      this.create_ngl_child_view,
      this.remove_ngl_child_view,
      this
    );
    this.model.on('msg:custom', this.handle_custom_message.bind(this));
    this.model.on('change:representations', this.representations_changed, this);
    this.model.on('change:visible', this.visible_changed.bind(this));
    this.model.on('change:positions', this.positions_changed, this);
    this.model.on('change:name', (event: any) => {
      if (this.component_obj) {
        this.component_obj.setName(event.changed.name);
      }
    });
    this.model.on('change:scale', (event: any) => {
      if (this.component_obj) {
        this.component_obj.setScale(event.changed.scale);
      }
    });
    this.model.on('change:quaternion', (event: any) => {
      if (this.component_obj) {
        this.component_obj.setRotation(event.changed.quaternion);
      }
    });
  }

  parameter_names(): Array<string> {
    return ['name', 'visible'];
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

  positions_changed() {
    var positions: any = this.model.get('positions');
    if (positions) {
      this.component_obj.updatePosition(positions);
    }
  }

  async representations_changed() {
    this.in_representations_changing = true;

    let views = await this.representationViews.update(this.model.get('representations'));

    for (let view of views) {
      await view.render();
    }

    this.in_representations_changing = false;
  }

  visible_changed() {
    if (this.component_obj) {
      this.component_obj.setVisibility(this.model.get('visible'));
    }
  }

  handle_custom_message(content: any, buffers: any): void {
    if (this.component_obj) {
      switch (content.do) {
        case 'auto_view':
          this.component_obj.autoView(content.duration || 0);
          break;
      }
    }
  }

  wire_component(): void {
	  this.representations_changed();

    if (this.component_obj.name != this.model.get('name')) {
      this.model.set('name', this.component_obj.name);
      this.model.save_changes();
    }

    this.component_obj.setScale(this.model.get('scale'));

    this.component_obj.signals.nameChanged.add((name: string): void => {
      this.model.set('name', name);
      this.model.save_changes();
    });
  }

  create_ngl_child_view(model: any, index: any) {
    return this.create_child_view(model, {
      stage_obj: this.stage_obj,
      component_obj: this.component_obj
    });
  }

  remove_ngl_child_view(view: any) {
    view.remove();
  }
}


export class StructureModel extends ComponentModel {
  defaults() {
    return {
      ...super.defaults(),

      ext: null,
      value: null,
      as_trajectory: false,
      trajectories: [],

      _model_name: 'StructureModel',
      _view_name: 'StructureView'
    };
  }

  static serializers: ISerializers = {
    trajectories: { deserialize: widgets.unpack_models },
    ...ComponentModel.serializers,
  };
}

export class StructureView extends ComponentView {
  trajectory_views: any;

  initialize(parameters: any): void {
    super.initialize(parameters);
    this.trajectory_views = new widgets.ViewList(
      this.create_ngl_child_view,
      this.remove_ngl_child_view,
      this
    );
    this.model.on('change:trajectories', this.change_trajectories, this);
  }

  parameter_names(): Array<string> {
    return super.parameter_names().concat(['ext', 'as_trajectory']);
  }

  async change_trajectories() {
    let views = await this.trajectory_views.update(this.model.get('trajectories'));

    for (let view of views) {
      await view.render();
    }
  }

  async render() {
    super.render();
    if (this.stage_obj && !this.component_obj && !this.rendered) {
      this.rendered = true;
      var value: any = this.model.get('value');
      if (this.model.get('ext')) {
      	value = new Blob([value],
      	                 { type: (typeof value === 'string' || value instanceof String)
      	                            ? 'text/plain'
      	                            : 'application/octet-binary' });
      }
      this.component_obj = await this.stage_obj.loadFile(value, this.get_parameters());
  	  this.change_trajectories();
  	  this.wire_component();
    }
  }

  remove() {
    super.remove();
    if (this.stage_obj && this.component_obj) {
      this.stage_obj.removeComponent(this.component_obj);
      this.component_obj = null;
    }
  }
}


