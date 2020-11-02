import {
  ISerializers,
  WidgetModel,
  WidgetView,
} from '@jupyter-widgets/base';

// eslint-disable-next-line @typescript-eslint/no-var-requires
const widgets = require('@jupyter-widgets/base');

import { MODULE_NAME, MODULE_VERSION } from './version';

// Import the CSS
import '../css/widget.css';

// eslint-disable-next-line @typescript-eslint/no-var-requires
//const NGL = require('ngl');


export class ComponentModel extends WidgetModel {
  defaults() {
    return {
      ...super.defaults(),

      uuid: null,

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
  }

  async representations_changed() {
    this.in_representations_changing = true;

    let views = await this.representationViews.update(this.model.get('representations'));

    for (let view of views) {
      await view.render();
    }

    this.in_representations_changing = false;
  }

  handle_custom_message(content: any): void {
    if (this.stage_obj) {
    }
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

      _model_name: 'StructureModel',
      _view_name: 'StructureView'
    };
  }

  static serializers: ISerializers = {
    ...ComponentModel.serializers,
  };
}

export class StructureView extends ComponentView {

  initialize(parameters: any): void {
    super.initialize(parameters);
  }

  render() {
    super.render();
    if (this.stage_obj && !this.component_obj && !this.rendered) {
      this.rendered = true;
      this.stage_obj.loadFile(this.model.get('value'))//, { defaultRepresentation: true })
      	.then((component: any) => {
      	  this.component_obj = component;
      	  this.representations_changed();
      	});
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

