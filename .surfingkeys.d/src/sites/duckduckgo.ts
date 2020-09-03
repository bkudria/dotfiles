import { Site } from './types';

const duckduckgo: Site = {
  domain: 'duckduckgo.com',
  engines: [
    {
      name: 'DuckDuckGo',
      alias: 'ddg',
      single: 'd',
      search: 'https://duckduckgo.com/?q=',
      completion: 'https://duckduckgo.com/ac/?q=',
      callback: response =>
        JSON.parse(response.text).map((r: { phrase: any }) => r.phrase),
    },
    {
      name: 'DuckDuckGo!',
      alias: 'ddl',
      single: 'l',
      search: 'https://duckduckgo.com/?q=\\',
      completion: 'https://duckduckgo.com/ac/?q=\\',
      callback: response =>
        JSON.parse(response.text).map((r: { phrase: any }) => r.phrase),
    },
  ],
};
export default duckduckgo;
