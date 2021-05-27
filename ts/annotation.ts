import {
  WidgetModel,
  WidgetView,
} from '@jupyter-widgets/base';

// eslint-disable-next-line @typescript-eslint/no-var-requires
const NGL = require('ngl');

import { MODULE_NAME, MODULE_VERSION } from './version';


export class AnnotationModel extends WidgetModel {
  defaults() {
    return {
      ...super.defaults(),

      visible: true,
      content: "",
      position: [0.0, 0.0, 0.0],
      offset: [0.0, 0.0, 0.0],

      _model_name: 'AnnotationModel',
      _view_name: 'AnnotationView',
      _model_module: MODULE_NAME,
      _model_module_version: MODULE_VERSION,
      _view_module: MODULE_NAME,
      _view_module_version: MODULE_VERSION,
    };
  }
}

export class AnnotationView extends WidgetView {
  component_obj: any;
  annotation_obj: any = null;

  initialize(parameters: any): void {
    super.initialize(parameters);
    this.component_obj = this.options.component_obj;

    this.model.on('change:content', this.content_changed, this);
    this.model.on('change:offset', this.offset_changed, this);
    this.model.on('change:position', this.position_changed, this);
    this.model.on('change:visible', this.visible_changed, this);
  }

  async content_changed() {
    if (this.annotation_obj) {
      var t = document.createElement('div');
      t.innerHTML = this.model.get('content');
      this.annotation_obj.setContent(t);
    }
  }

  async offset_changed() {
    if (this.annotation_obj) {
      const offset = this.model.get('offset');
      this.annotation_obj.offsetX = offset[0];
      this.annotation_obj.offsetY = offset[1];
      this.annotation_obj._update();
    }
  }

  async position_changed() {
    if (this.annotation_obj) {
      this.annotation_obj.position.fromArray(this.model.get('position'));
      this.annotation_obj._update();
    }
  }

  async visible_changed() {
    if (this.annotation_obj) {
      this.annotation_obj.setVisibility(this.model.get('visible'));
    }
  }

  async remove() {
    super.remove();
    if (this.component_obj && this.annotation_obj) {
      this.component_obj.removeAnnotation(this.annotation_obj);
      this.annotation_obj = null;
    }
  }

  async render() {
    if (!this.component_obj) {
      this.el.textContent = "Add to component widget to visualize."
    } else if (!this.annotation_obj) {
      super.render();
      const position = this.model.get('position');
      const offset = this.model.get('offset');
      var t = document.createElement('div');
      t.innerHTML = this.model.get('content');
      this.annotation_obj = this.component_obj.addAnnotation(
        new NGL.Vector3(position[0], position[1], position[2]),
        t,
        {
        	offsetX: offset[0],
        	offsetY: offset[1],
        	visible: this.model.get('visible')
        });
    }
  }
}
