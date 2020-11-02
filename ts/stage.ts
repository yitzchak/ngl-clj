import {
  DOMWidgetModel,
  DOMWidgetView,
  ISerializers,
  //WidgetModel,
  //WidgetView,
} from '@jupyter-widgets/base';

// eslint-disable-next-line @typescript-eslint/no-var-requires
const widgets = require('@jupyter-widgets/base');

import { MODULE_NAME, MODULE_VERSION } from './version';

// Import the CSS
import '../css/widget.css';

// eslint-disable-next-line @typescript-eslint/no-var-requires
const NGL = require('ngl');


export class StageModel extends DOMWidgetModel {
  defaults() {
    return {
      ...super.defaults(),

      impostor: true,
      quality: 'medium',
      worker_default: true,
      sample_level: 0,
      background_color: 'black',
      rotate_speed: 2.0,
      zoom_zpeed: 1.2,
      pan_speed: 1.0,
      clip_near: 0,
      clip_far: 100,
      clip_dist: 10,
      clip_mode: 'scene',
      clip_scale: 'relative',
      fog_near: 50,
      fog_far: 100,
      camera_fov: 40,
      camera_eye_sep: 0.3,
      camera_type: 'perspective',
      light_color: '#dddddd',
      light_intensity: 1.0,
      ambient_color: '#dddddd',
      ambient_intensity: 0.2,
      hover_timeout: 0,
      tooltip: true,
      mouse_preset: 'default',

      components: [],

      _model_name: 'StageModel',
      _model_module: MODULE_NAME,
      _model_module_version: MODULE_VERSION,
      _view_name: 'StageView',
      _view_module: MODULE_NAME,
      _view_module_version: MODULE_VERSION,
    };
  }

  static serializers: ISerializers = {
    components: { deserialize: widgets.unpack_models },
    ...DOMWidgetModel.serializers,
  };
}

export class StageView extends DOMWidgetView {
  stage_obj: any;
  componentViews: any;
  in_components_changing = false;


  initialize(parameters: any): void {
    super.initialize(parameters);
    this.componentViews = new widgets.ViewList(
      this.create_ngl_child_view,
      this.remove_ngl_child_view,
      this
    );
    this.model.on('msg:custom', this.handle_custom_message.bind(this));
    this.model.on('change:components', this.components_changed, this);
    this.model.on_some_change([
      'impostor',
      'quality',
      'worker_default',
      'sample_level',
      'background_color',
      'rotate_speed',
      'zoom_zpeed',
      'pan_speed',
      'clip_near',
      'clip_far',
      'clip_dist',
      'clip_mode',
      'clip_scale',
      'fog_near',
      'fog_far',
      'camera_fov',
      'camera_eye_sep',
      'camera_type',
      'light_color',
      'light_intensity',
      'ambient_color',
      'ambient_intensity',
      'hover_timeout',
      'tooltip',
      'mouse_preset'
    ], this.setParameters.bind(this), this);
  }

  async components_changed() {
    this.in_components_changing = true;

    let views = await this.componentViews.update(this.model.get('components'));

    for (let view of views) {
      await view.render();
    }

    this.in_components_changing = false;
  }

  handle_custom_message(content: any): void {
    if (this.stage_obj) {
    }
  }

  stage_parameters(): any {
    return {
      impostor: this.model.get('impostor'),
      quality: this.model.get('quality'),
      workerDefault: this.model.get('worker_default'),
      sampleLevel: this.model.get('sample_Level'),
      backgroundColor: this.model.get('background_color'),
      rotateSpeed: this.model.get('rotate_speed'),
      zoomSpeed: this.model.get('zoom_speed'),
      panSpeed: this.model.get('pan_speed'),
      clipNear: this.model.get('clip_near'),
      clipFar: this.model.get('clip_far'),
      clipDist: this.model.get('clip_dist'),
      clipMode: this.model.get('clip_mode'),
      clipScale: this.model.get('clip_scale'),
      fogNear: this.model.get('fog_near'),
      fogFar: this.model.get('fog_far'),
      cameraFov: this.model.get('camera_fov'),
      cameraEyeSep: this.model.get('camera_eye_sep'),
      cameraType: this.model.get('camera_type'),
      lightColor: this.model.get('light_color'),
      lightIntensity: this.model.get('light_intensity'),
      ambientColor: this.model.get('ambient_color'),
      ambientIntensity: this.model.get('ambient_intensity'),
      hoverTimeout: this.model.get('hover_timeout'),
      tooltip: this.model.get('tooltip'),
      mousePreset: this.model.get('mouse_preset')
    }
  }

  handleEvent(event: Event): void {
    if (event.type === 'contextmenu') event.stopPropagation();
  }

  render() {
    super.render();
    this.displayed.then(() => {
      this.el.classList.add('ngl-stage');
      this.el.classList.add('jupyter-widgets');

      this.stage_obj = new NGL.Stage(this.el, this.stage_parameters());
      this.components_changed();
    });
  }

  setParameters(): void {
    if (this.stage_obj) {
      this.stage_obj.setParameters(this.stage_parameters());
    }
  }

  processPhosphorMessage(msg: any): void {
    super.processPhosphorMessage(msg);
    if ((msg.type === 'resize' || msg.type === 'after-show') && this.stage_obj) {
      this.stage_obj.handleResize();
    }
  }

  create_ngl_child_view(model: any, index: any) {
    return this.create_child_view(model, {
      stage_obj: this.stage_obj
    });
  }

  remove_ngl_child_view(view: any) {
    view.remove();
  }
}

