import { openStoryAndComments } from '../utils';
import { Site } from './types';

const lobsters: Site = {
  domain: 'lobste.rs',
  mappings: [
    {
      path: /^(page.*)?$/,
      keys: 'f',
      description: '#1Open Link and Comments [Lobsters]',
      fn: () =>
        openStoryAndComments({
          story: 'li.story',
          link: 'a.u-url',
          comments: '.comments_label a',
        }),
    },
  ],
};

export default lobsters;
