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

import { create_buffer, camel_object } from './utils';


export class RepresentationModel extends WidgetModel {
  defaults(): any {
    return {
      ...super.defaults(),

      clip_center: [0, 0, 0],
      clip_near: 0,
      clip_radius: 0,
      color_domain: undefined,
      color_mode: 'hcl',
      color_reverse: false,
      color_scale: '',
      color_scheme: "chainname",
      color_value: 0x909090,
      depth_write: true,
      diffuse: 0xffffff,
      diffuse_interior: false,
      disable_impostor: false,
      disable_picking: false,
      flat_shaded: false,
      interior_color: 0x222222,
      interior_darkening: 0,
      lazy: false,
      //matrix: [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]],
      metalness: 0.0,
      name: null,
      opacity: 1.0,
      open_ended: true,
      quality: undefined,
      radial_segments: 10,
      roughness: 0.4,
      side: 'double',
      sphere_detail: 1,
      use_interior_color: false,
      visible: true,
      wireframe: false,

      _type: "",
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
    this.model.on('change', this.parameters_changed.bind(this));
    this.model.on('change:visible', this.visible_changed.bind(this));
  }

  visible_changed() {
    if (this.representation_obj) {
      this.representation_obj.setVisibility(this.model.get('visible'));
    }
  }

  handle_custom_message(content: any): void {
    if (this.stage_obj) {
    }
  }

  get_parameters(): any {
    return camel_object(this.model.attributes);
  }

  parameters_changed() {
    if (this.representation_obj) {
      try {
        this.representation_obj.setParameters(this.get_parameters());
        this.representation_obj.build();
      } catch (e) {
        console.log(this.get_parameters());
        console.log(this);
        console.log(e);
      }
    }
  }

  render() {
    super.render();
    if (this.component_obj && !this.representation_obj) {
      this.representation_obj =
        this.component_obj.addRepresentation(this.model.get('_type'),
                                             { sdf: false, ...this.get_parameters() });
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


export class BufferRepresentationModel extends RepresentationModel {
  defaults(): any {
    return {
      ...super.defaults(),

      _type: 'buffer',
      _model_name: 'BufferRepresentationModel',
      _view_name: 'BufferRepresentationView'
    };
  }
}


export class BufferRepresentationView extends RepresentationView {
  initialize(parameters: any): void {
    super.initialize(parameters);

    this.model.on('change:buffer', this.buffer_changed.bind(this));
  }

  buffer_changed() {
    if (this.representation_obj) {
      this.component_obj.removeRepresentation(this.representation_obj);
      this.representation_obj = null;
      this.render();
    }
  }

  render() {
    const buffer = this.model.get('buffer');

    if (this.component_obj && !this.representation_obj && buffer.type) {
      this.representation_obj =
        this.component_obj.addBufferRepresentation(create_buffer(buffer),
                                                   this.get_parameters());
    } else {
      super.render();
    }
  }
}


export class StructureRepresentationModel extends RepresentationModel {
  defaults(): any {
    return {
      ...super.defaults(),

      assembly: 'default',
      radius_scale: 1.0,
      sele: ""
    };
  }
}


export class CartoonModel extends StructureRepresentationModel {
  defaults(): any {
    return {
      ...super.defaults(),

      aspect_ratio: 5.0,
      subdiv: 12,
      radial_segments: 20,
      tension: null,
      capped: true,
      smooth_sheet: false,

      _type: 'cartoon',
      _model_name: 'CartoonModel'
    };
  }
}


export class BallAndStickModel extends StructureRepresentationModel {
  defaults(): any {
    return {
      ...super.defaults(),

      //aspect_ratio: 2.0,
      //bond_scale: 0.4,
      //bond_spacing: 1.0,
      // cylinder_only: false,
      //disable_impostor: true,
      // line_only: false,
      // linewidth: 2,
      multiple_bond: 'off',
      //open_ended: true,
      //radial_segments: 10,
      //sphere_detail: 2,

      _type: 'ball+stick',
      _model_name: 'BallAndStickModel'
    };
  }
}


export class BackboneModel extends BallAndStickModel {
  defaults(): any {
    return {
      ...super.defaults(),

      _type: 'backbone',
      _model_name: 'BackboneModel'
    };
  }
}


export class BaseModel extends BallAndStickModel {
  defaults(): any {
    return {
      ...super.defaults(),

      _type: 'base',
      _model_name: 'BaseModel'
    };
  }
}


export class LicoriceModel extends BallAndStickModel {
  defaults(): any {
    return {
      ...super.defaults(),

      _type: 'licorice',
      _model_name: 'LicoriceModel'
    };
  }
}


export class LineModel extends StructureRepresentationModel {
  defaults(): any {
    return {
      ...super.defaults(),

      _type: 'line',
      _model_name: 'LineModel'
    };
  }
}


export class MeasurementRepresentationModel extends RepresentationModel {
  defaults(): any {
    return {
      ...super.defaults(),

      label_size: 2.0,
      label_color: "white",
      label_visible: true,
      label_z_offset: 0.5
    };
  }
}


export class DihedralModel extends MeasurementRepresentationModel {
  defaults(): any {
    return {
      ...super.defaults(),

      atom_quad: [],

      _type: 'dihedral',
      _model_name: 'DihedralModel'
    };
  }
}


export class RibbonModel extends StructureRepresentationModel {
  defaults(): any {
    return {
      ...super.defaults(),

      _type: 'ribbon',
      _model_name: 'RibbonModel'
    };
  }
}


export class SpacefillModel extends StructureRepresentationModel {
  defaults(): any {
    return {
      ...super.defaults(),

      _type: 'spacefill',
      _model_name: 'SpacefillModel'
    };
  }
}


export class SurfaceModel extends StructureRepresentationModel {
  defaults(): any {
    return {
      ...super.defaults(),

      background: false,
      color_volume: null,
      contour: false,
      cutoff: 0.0,
      filter_sele: "",
      opaque_back: true,
      probe_radius: 1.4,
      scale_factor: 2.0,
      smooth: 2,
      surface_type: 'ms',
      use_worker: true,

      _type: 'surface',
      _model_name: 'SurfaceModel'
    };
  }
}


export class UnitcellModel extends StructureRepresentationModel {
  defaults(): any {
    return {
      ...super.defaults(),

      radius_size: 0.5,

      _type: 'unitcell',
      _model_name: 'UnitcellModel'
    };
  }
}



