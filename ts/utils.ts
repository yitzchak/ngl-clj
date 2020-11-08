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


