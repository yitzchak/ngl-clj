import {
  ISerializers,
  WidgetModel,
  WidgetView,
} from '@jupyter-widgets/base';

// eslint-disable-next-line @typescript-eslint/no-var-requires
const widgets = require('@jupyter-widgets/base');

import { MODULE_NAME, MODULE_VERSION } from './version';

// eslint-disable-next-line @typescript-eslint/no-var-requires
const NGL = require('ngl');

import { camel } from 'case';

import { ViewSet, create_buffer } from './utils';


export class ComponentModel extends WidgetModel {
  defaults() {
    return {
      ...super.defaults(),

      auto_view_duration: null,
      visible: true,
      name: null,
      position: [0.0, 0.0, 0.0],
      positions: null,
      quaternion: [0.0, 0.0, 0.0, 0.0],
      scale: 1.0,
      annotations: [],
      representations: [],

      _model_module: MODULE_NAME,
      _model_module_version: MODULE_VERSION,
      _view_module: MODULE_NAME,
      _view_module_version: MODULE_VERSION,
    };
  }

  static serializers: ISerializers = {
    annotations: { deserialize: widgets.unpack_models },
    representations: { deserialize: widgets.unpack_models },
    positions: {
      deserialize: (value: any): Float32Array | null => {
        return (value instanceof DataView) ? new Float32Array(value.buffer) : value;
      },
      serialize: (value: any): DataView | null => {
        return (value instanceof Float32Array) ? new DataView(value.buffer) : value;
      }
    },
    ...WidgetModel.serializers,
  };
}

export class ComponentView extends WidgetView {
  stage_obj: any;
  component_obj: Promise<any> | null = null;
  annotationViews: any;
  representationViews: any;
  in_annotations_changing = false;
  in_representations_changing = false;
  rendered = false;

  initialize(parameters: any): void {
    super.initialize(parameters);
    this.stage_obj = this.options.stage_obj;

    this.annotationViews = new ViewSet(
      this.create_ngl_child_view,
      this.remove_ngl_child_view,
      this
    );
    this.representationViews = new ViewSet(
      this.create_ngl_child_view,
      this.remove_ngl_child_view,
      this
    );
    this.model.on('msg:custom', this.handle_custom_message.bind(this));
    this.model.on('change:annotations', this.annotations_changed, this);
    this.model.on('change:representations', this.representations_changed, this);
    this.model.on('change:visible', this.visible_changed.bind(this));
    this.model.on('change:positions', this.positions_changed, this);
    this.model.on('change:name', async (event: any) => {
      if (this.component_obj) {
        (await this.component_obj).setName(event.changed.name);
      }
    });
    this.model.on('change:position', async (event: any) => {
      if (this.component_obj) {
        let component_obj = await this.component_obj

        component_obj.setPosition(event.changed.position);
        component_obj.updateRepresentations({ position: true });
      }
    });
    this.model.on('change:scale', async (event: any) => {
      if (this.component_obj) {
        (await this.component_obj).setScale(event.changed.scale);
      }
    });
    this.model.on('change:quaternion', async (event: any) => {
      if (this.component_obj) {
        (await this.component_obj).setRotation(event.changed.quaternion);
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
        params[camel(name)] = value;
      }
    }

    return params;
  }

  async positions_changed() {
    var positions: any = this.model.get('positions');
    let component_obj = await this.component_obj;
    if (positions) {
      component_obj.structure.updatePosition(positions);
      component_obj.updateRepresentations({ position: true });
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

  async annotations_changed() {
    this.in_annotations_changing = true;

    let views = await this.annotationViews.update(this.model.get('annotations'));

    for (let view of views) {
      await view.render();
    }

    this.in_annotations_changing = false;
  }

  async visible_changed() {
    if (this.component_obj) {
      (await this.component_obj).setVisibility(this.model.get('visible'));
    }
  }

  async handle_custom_message(content: any, buffers: DataView[]): Promise<void> {
    if (this.component_obj) {
      switch (content.do) {
        case 'auto_view':
          (await this.component_obj).autoView(content.duration || 0);
          break;
        case 'update_position':
          (await this.component_obj).structure.updatePosition(new Float32Array(buffers[0].buffer));
          (await this.component_obj).updateRepresentations({ position: true });
          break;
        case 'remove_all_measurements':
          (await this.component_obj).removeAllMeasurements();
          break;
        case 'remove_measurement':
          (await this.component_obj).removeMeasurement(content.atoms);
          break;
        case 'add_measurement':
          (await this.component_obj).addMeasurement(content.atoms);
          break;
        case 'move':
          this.stage_obj.animationControls.moveComponent(await this.component_obj, content.to, content.duration || 0);
          break;
        case 'rock':
          this.stage_obj.animationControls.rockComponent(await this.component_obj, content.axis, content.angle, content.end, content.duration || 0);
          break;
        case 'spin':
          this.stage_obj.animationControls.spinComponent(await this.component_obj, content.axis, content.angle, content.duration || 0);
          break;
      }
    }
  }

  async wire_component(): Promise<void> {
    if (this.model.get('positions')) {
      this.positions_changed();
    }

    let component_obj = await this.component_obj;

    component_obj.setPosition(this.model.get('position'));
    component_obj.setScale(this.model.get('scale'));

    this.annotations_changed();
    this.representations_changed();

    let auto_view_duration = this.model.get('auto_view_duration');

    if (auto_view_duration !== null) {
      component_obj.autoView(auto_view_duration);
    }

    if (component_obj.name != this.model.get('name')) {
      this.model.set('name', component_obj.name);
      this.model.save_changes();
    }

    component_obj.signals.nameChanged.add((name: string): void => {
      this.model.set('name', name || null);
      this.model.save_changes();
    });
  }

  async remove() {
    super.remove();
    if (this.stage_obj && this.component_obj) {
      this.stage_obj.removeComponent(await this.component_obj);
      this.component_obj = null;
    }
  }

  async create_ngl_child_view(model: any) {
    return this.create_child_view(model, {
      stage_obj: this.stage_obj,
      component_obj: await this.component_obj
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
      sele: "",
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
    this.trajectory_views = new ViewSet(
      this.create_ngl_child_view,
      this.remove_ngl_child_view,
      this
    );
    this.model.on('change:trajectories', this.change_trajectories, this);
  }

  parameter_names(): Array<string> {
    return super.parameter_names().concat(['ext', 'as_trajectory', 'sele']);
  }

  async change_trajectories() {
    let views = await this.trajectory_views.update(this.model.get('trajectories'));

    for (let view of views) {
      await view.render();
    }
  }

  async load_file(): Promise<any> {
    var value: any = this.model.get('value');

    if (this.model.get('ext')) {
      value = new Blob([value],
        {
          type: (typeof value === 'string' || value instanceof String)
            ? 'text/plain'
            : 'application/octet-binary'
        });
    }

    return this.stage_obj.loadFile(value, this.get_parameters());
  }

  async render() {
    if (!this.stage_obj) {
      this.el.textContent = "Add to stage widget to visualize."
    } else if (!this.component_obj) {
      super.render();
      this.component_obj = this.load_file();
      this.change_trajectories();
      this.wire_component();
    }
  }

  async wire_component(): Promise<void> {
    await super.wire_component();
    var component_obj = await this.component_obj;
    component_obj.spacefillRepresentation.signals.parametersChanged.add((value: any) => {
      if (value.sele && value.sele[0] === '@') {
        var atomProxy = component_obj.structure.getAtomProxy();
        this.options.parent.send({
          event: 'select',
          data: value.sele.substring(1).split(',').map((indexStr: string): any => {
            atomProxy.index = parseInt(indexStr);
            return atomProxy.toObject();
          })
        });
      }
    });
  }
}


export class ShapeModel extends ComponentModel {
  defaults() {
    return {
      ...super.defaults(),

      primitives: [],

      _model_name: 'ShapeModel',
      _view_name: 'ShapeView'
    };
  }
}

export class ShapeView extends ComponentView {

  initialize(parameters: any): void {
    super.initialize(parameters);
    this.model.on('change:primitives', this.change_primitives, this);
  }

  async change_primitives() {
    if (this.component_obj) {
      this.stage_obj.removeComponent(this.component_obj);
      this.component_obj = null;
    }
    await this.render();
  }

  async render() {
    if (!this.component_obj) {
      super.render();
      let shape = new NGL.Shape(this.model.get('name'));
      for (const primitive of this.model.get('primitives')) {
        if (primitive.buffer) {
          shape.addBuffer(create_buffer(primitive));
        } else {
          switch (primitive.type) {
            case 'arrow':
              shape.addArrow(primitive.position1, primitive.position2, primitive.color, primitive.radius, primitive.name);
              break;
            case 'box':
              shape.addBox(primitive.position, primitive.color, primitive.size, primitive.height_axis, primitive.depth_axis, primitive.name);
              break;
            case 'cone':
              shape.addCone(primitive.position1, primitive.position2, primitive.color, primitive.radius, primitive.name);
              break;
            case 'cylinder':
              shape.addCylinder(primitive.position1, primitive.position2, primitive.color, primitive.radius, primitive.name);
              break;
            case 'ellipsoid':
              shape.addEllipsoid(primitive.position, primitive.color, primitive.radius, primitive.major_axis, primitive.minor_axis, primitive.name);
              break;
            case 'mesh':
              shape.addMesh(primitive.position, primitive.color, primitive.index, primitive.normal, primitive.name);
              break;
            case 'octahedron':
              shape.addOctahedron(primitive.position, primitive.color, primitive.size, primitive.height_axis, primitive.depth_axis, primitive.name);
              break;
            case 'sphere':
              shape.addSphere(primitive.position, primitive.color, primitive.radius, primitive.name);
              break;
            case 'tetrahedron':
              shape.addTetrahedron(primitive.position, primitive.color, primitive.size, primitive.height_axis, primitive.depth_axis, primitive.name);
              break;
            case 'text':
              shape.addText(primitive.position, primitive.color, primitive.size, primitive.text);
              break;
            case 'torus':
              shape.addTorus(primitive.position, primitive.color, primitive.radius, primitive.major_axis, primitive.minor_axis, primitive.name);
              break;
          }
        }
      }
      this.component_obj = this.stage_obj.addComponentFromObject(shape, this.get_parameters());
      this.wire_component();
    }
  }
}
