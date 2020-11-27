import { snake } from 'case';


export function serialize_model(model: any, object: any): any {
  var result: any = {};

  for (const [key, model_key] of Object.entries(object)) {
    const value = model.get(model_key || snake(key));
    if (value != null) {
      result[key] = value;
    }
  }

  return result;
}


export function snake_object(object: any): any {
  var result: any = {};

  for (const [key, value] of Object.entries(object)) {
    result[snake(key)] = value;
  }

  return result;
}

export class ViewSet<T> {
  constructor(
    create_view: (model: any) => T | Promise<T>,
    remove_view: ((view: T) => void) | null,
    context: any
  ) {
    this._handler_context = context || this;
    this._create_view = create_view;
    this._remove_view =
      remove_view ||
      function(view): void {
        (view as any).remove();
      };
  }

  async update(new_models: any[]): Promise<T[]> {
    for (let [model, view] of this._model_views) {
      if (!new_models.includes(model)) {
        this._remove_view.call(this._handler_context, view);
        this._model_views.delete(model);
      }
    }

    let views = [];

    for (let model of new_models) {
      if (!this._model_views.has(model)) {
        let view = await this._create_view.call(this._handler_context, model);
        views.push(view);
        this._model_views.set(model, view);
      }
    }

    return views;
  }

  remove(): void {
    for (let view of this._model_views.values()) {
      this._remove_view.call(this._handler_context, view);
    }
    this._model_views.clear();
  }

  dispose(): void {
    this._model_views.clear();
  }

  _handler_context: any;
  _model_views: Map<any, any> = new Map();
  _create_view: (model: any) => T | Promise<T>;
  _remove_view: (view: T) => void;
}

