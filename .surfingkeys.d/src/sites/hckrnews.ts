import { openStoryAndComments } from '../utils';

import { Site } from './types';

const hckrnews: Site = {
  domain: 'hckrnews.com',
  mappings: [
    {
      path: RegExp(''),
      keys: 'f',
      description: '#1Open Link and Comments [HckrNews]',
      fn: () =>
        openStoryAndComments({
          story: 'li.entry.row',
          link: 'a.link.story',
          comments: 'a.hn.story',
        }),
    },
  ],
};

export default hckrnews;
