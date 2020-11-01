import {
  DOMWidgetModel,
  DOMWidgetView,
  ISerializers,
  WidgetModel,
  WidgetView,
} from '@jupyter-widgets/base';

// eslint-disable-next-line @typescript-eslint/no-var-requires
const widgets = require('@jupyter-widgets/base');

import { MODULE_NAME, MODULE_VERSION } from './version';

// Import the CSS
import '../css/widget.css';

import ngl from 'ngl';


export class NGLModel extends DOMWidgetModel {
  defaults() {
    return {
      ...super.defaults(),

      _model_name: 'NGLModel',
      _model_module: MODULE_NAME,
      _model_module_version: MODULE_VERSION,
      _view_name: 'NGLView',
      _view_module: MODULE_NAME,
      _view_module_version: MODULE_VERSION,
    };
  }

  static serializers: ISerializers = {
    context_menus: { deserialize: widgets.unpack_models },
    elements: { deserialize: widgets.unpack_models },
    graph_layouts: { deserialize: widgets.unpack_models },
    ...DOMWidgetModel.serializers,
  };
}

export class NGLView extends DOMWidgetView {
  ngl_obj: any;
  ngl_container: any;

  initialize(parameters: any): void {
    super.initialize(parameters);
    this.model.on('msg:custom', this.handle_custom_message.bind(this));
  }

  handle_custom_message(content: any): void {
    if (this.ngl_obj) {
    }
  }

  handleEvent(event: Event): void {
    if (event.type === 'contextmenu') event.stopPropagation();
  }

  render() {
    super.render();
    this.displayed.then(() => {
    });
  }

  processPhosphorMessage(msg: any): void {
    super.processPhosphorMessage(msg);
    if ((msg.type === 'resize' || msg.type === 'after-show') && this.ngl_obj) {
    }
  }

  create_ngl_child_view(model: any, index: any) {
    return this.create_child_view(model, {
      ngl_obj: this.ngl_obj
    });
  }

  remove_ngl_child_view(view: any) {
    view.remove();
  }
}

