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

import { snake_object, ViewSet } from './utils';


export class StageModel extends DOMWidgetModel {
  defaults() {
    return {
      ...super.defaults(),

      ambient_color: '#dddddd',
      ambient_intensity: 0.2,
      background_color: 'black',
      camera_eye_sep: 0.3,
      camera_fov: 40,
      camera_type: 'perspective',
      clip_dist: 10,
      clip_far: 100,
      clip_mode: 'scene',
      clip_near: 0,
      clip_scale: 'relative',
      components: [],
      fog_far: 100,
      fog_near: 50,
      fullscreen: false,
      hover_timeout: 0,
      impostor: true,
      light_color: '#dddddd',
      light_intensity: 1.0,
      mouse_preset: 'default',
      pan_speed: 1.0,
      pick_filter: ["click"],
      quality: 'medium',
      rock: false,
      rotate_speed: 2.0,
      sample_level: 0,
      spin: false,
      stage_tooltip: true,
      worker_default: true,
      zoom_zpeed: 1.2,

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

function atom_proxy_to_object(atom: any): any {
  return snake_object(atom.toObject());
}

function bond_proxy_to_object(bond: any): any {
  return {
    atom1: atom_proxy_to_object(bond.atom1),
    atom2: atom_proxy_to_object(bond.atom2),
    ...snake_object(bond.toObject())
  };
}

export class StageView extends DOMWidgetView {
  stage_container: any;
  stage_obj: any;
  componentViews: any;
  in_components_changing = false;


  initialize(parameters: any): void {
    super.initialize(parameters);
    this.componentViews = new ViewSet(
      this.create_ngl_child_view,
      this.remove_ngl_child_view,
      this
    );
    this.model.on('msg:custom', this.handle_custom_message.bind(this));
    this.model.on('change:components', this.components_changed, this);

    this.model.on('change:fullscreen', () => {
      if (this.stage_obj) {
        this.stage_obj.toggleFullscreen();
      }
    });

    this.model.on('change:spin', (event: any) => {
      if (this.stage_obj) {
        this.stage_obj.setSpin(event.changed.spin);
      }
    })

    this.model.on('change:rock', (event: any) => {
      if (this.stage_obj) {
        this.stage_obj.setRock(event.changed.rock);
      }
    })

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
      'stage_tooltip',
      'mouse_preset'
    ], this.setParameters.bind(this), this);

    this.on('remove', () => {
      this.send({ event: 'remove' });
    });
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
      switch (content.do) {
        case 'make_image':
          this.make_image(content);
          break;
        case 'auto_view':
          this.stage_obj.autoView(content.duration || 0);
          break;
        case 'move':
          this.stage_obj.animationControls.move(content.to, content.duration || 0);
          break;
      }
    }
  }

  async make_image(params: any) {
    const blob: Blob = await this.stage_obj.makeImage(params);
    this.send({ event: 'image', type: blob.type, uuid: params.uuid },
              [await blob.arrayBuffer()]);
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
      tooltip: this.model.get('stage_tooltip'),
      mousePreset: this.model.get('mouse_preset')
    }
  }

  render() {
    super.render();
    this.displayed.then(() => {
      this.el.classList.add('ngl-stage');
      this.el.classList.add('jupyter-widgets');
      this.el.setAttribute('data-jp-suppress-context-menu', '');

      this.stage_container = document.createElement('div');
      this.el.appendChild(this.stage_container);

      this.stage_obj = new NGL.Stage(this.stage_container, this.stage_parameters());
      document.body.appendChild(this.stage_obj.tooltip);
      this.components_changed();
      if (this.model.get('spin')) {
        this.stage_obj.setSpin(true);
      }
      if (this.model.get('rock')) {
        this.stage_obj.setRock(true);
      }
      this.stage_obj.signals.fullscreenChanged.add((value: boolean) => {
        this.model.set('fullscreen', value);
        this.model.save_changes();
      });
      this.stage_obj.signals.clicked.add((picked: any) => this.on_pick('click', picked));
      this.stage_obj.signals.hovered.add((picked: any) => this.on_pick('hover', picked));
    });
  }

  on_pick(signal: string, picked: any): void {
    if (!this.model.get('pick_filter').includes(signal)) return;

    var data: any = {
      type: null,
      signal
    };

    if (picked) {
      data.type = picked.type;
      data.alt_key = picked.altKey;
      data.ctrl_key = picked.ctrlKey;
      data.meta_key = picked.metaKey;
      data.shift_key = picked.shiftKey;

      if (picked.atom) {
        data.atom = atom_proxy_to_object(picked.atom);
      }

      if (picked.bond) {
        data.bond = bond_proxy_to_object(picked.bond);
      }

      if (picked.closestBondAtom) {
        data.closest_bond_atom = atom_proxy_to_object(picked.closestBondAtom);
      }

      if (picked.component) {
        data.component = picked.component.name;
      }

      if (picked.contact) {
        data.contact = bond_proxy_to_object(picked.contact);
      }

      if (picked.distance) {
        data.distance = bond_proxy_to_object(picked.distance);
      }

      if (picked.position) {
        data.position = snake_object(picked.position);
      }
    }

    this.send({ event: 'pick', data });
  }

  setParameters(): void {
    if (this.stage_obj) {
      this.stage_obj.setParameters(this.stage_parameters());
    }
  }

  processLuminoMessage(msg: any): void {
    super.processLuminoMessage(msg);
    if ((msg.type === 'resize' || msg.type === 'after-show') && this.stage_obj) {
      const box = this.el.getBoundingClientRect();
      this.stage_obj.setSize(Math.floor(box.width) + 'px', Math.floor(box.height) + 'px');
    }
  }

  create_ngl_child_view(model: any) {
    return this.create_child_view(model, {
      stage_obj: this.stage_obj
    });
  }

  remove_ngl_child_view(view: any) {
    view.remove();
  }
}

