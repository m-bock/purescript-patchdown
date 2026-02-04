import * as jsyaml from "js-yaml";

export const yamlToJson = (yaml) => () => {
  if (/^\s*$/.test(yaml)) {
    return {};
  }
  const ret = jsyaml.load(yaml);
  if (typeof ret === "undefined") {
    throw(new Error("Invalid YAML"));
  }
  return ret;
};

export const printYaml = (json) => {
  return jsyaml.dump(json);
};
