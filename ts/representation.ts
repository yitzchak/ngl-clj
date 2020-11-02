import {
  //ISerializers,
  WidgetModel,
  WidgetView,
} from '@jupyter-widgets/base';

// eslint-disable-next-line @typescript-eslint/no-var-requires
//const widgets = require('@jupyter-widgets/base');

import { MODULE_NAME, MODULE_VERSION } from './version';

// Import the CSS
import '../css/widget.css';

// eslint-disable-next-line @typescript-eslint/no-var-requires
const camelcaseKeys = require('camelcase-keys');


export class RepresentationModel extends WidgetModel {
  defaults() {
    return {
      ...super.defaults(),

      _name: "",

      _model_module: MODULE_NAME,
      _model_module_version: MODULE_VERSION,
      _view_name: 'RepresentationView',
      _view_module: MODULE_NAME,
      _view_module_version: MODULE_VERSION,
    };
  }
}

export class RepresentationView extends WidgetView {
  stage_obj: any;
  component_obj: any;
  representation_obj: any;
  rendered = false;

  initialize(parameters: any): void {
    super.initialize(parameters);
    this.stage_obj = this.options.stage_obj;
    this.component_obj = this.options.component_obj;

    this.model.on('msg:custom', this.handle_custom_message.bind(this));
  }

  handle_custom_message(content: any): void {
    if (this.stage_obj) {
    }
  }

  render() {
    super.render();
    if (this.component_obj && !this.representation_obj) {
      var parameters: any = camelcaseKeys(this.model.attributes, { exclude: [/^_/] })
      console.log(parameters);
      this.representation_obj = this.component_obj.addRepresentation(this.model.get('_name'), parameters);
    }
  }

  remove() {
    super.remove();
    if (this.component_obj && this.representation_obj) {
      this.component_obj.removeRepresentation(this.representation_obj);
      this.representation_obj = null;
    }
  }
}


export class StructureRepresentationModel extends RepresentationModel {
  defaults() {
    return {
      ...super.defaults(),

      sele: ""
    };
  }
}


export class CartoonModel extends StructureRepresentationModel {
  defaults() {
    return {
      ...super.defaults(),

      _name: 'cartoon',

      _model_name: 'CartoonModel'
    };
  }
}


export class BallAndStickModel extends StructureRepresentationModel {
  defaults() {
    return {
      ...super.defaults(),

      _name: 'ball+stick',

      sphere_detail: 2,

      _model_name: 'BallAndStickModel'
    };
  }
}

