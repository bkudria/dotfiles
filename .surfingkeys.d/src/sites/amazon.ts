import { Site } from './types';

const amazon: Site = {
  domain: 'smile.amazon.com',
  engines: [
    {
      name: 'Amazon',
      alias: 'az',
      search: 'https://smile.amazon.com/s/?field-keywords=',
      completion:
        'https://completion.amazon.com/search/complete?method=completion&mkt=1&search-alias=aps&q=',
      callback: response => JSON.parse(response.text)[1],
    },
  ],
};

export default amazon;
