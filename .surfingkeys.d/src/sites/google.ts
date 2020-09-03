import { Site } from './types';

const google: Site = {
  domain: 'google.com',
  engines: [
    {
      name: 'Google',
      alias: 'go',
      search: 'https://www.google.com/search?q=',
      completion:
        'https://www.google.com/complete/search?client=chrome-omni&gs_ri=chrome-ext&oit=1&cp=1&pgcl=7&q=',
      callback: response => JSON.parse(response.text)[1],
    },
  ],
};

export default google;
