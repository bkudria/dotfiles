type Engine = {
  name: string;
  alias: string;
  single?: string;
  search: string;
  completion: string;
  callback: (response: { text: string }) => any;
};

type Mapping = {
  path: RegExp;
  keys: string;
  description: string;
  fn: () => any;
};

export type Site = {
  domain: string;
  engines?: Engine[];
  onLoad?: () => any;
  mappings?: Mapping[];
};
