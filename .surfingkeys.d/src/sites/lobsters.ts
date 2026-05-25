import { openStoryAndComments, renderGauges } from '../utils';
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
  onLoad: () => {
    renderGauges({
      rows: 'li.story',
      anchor: '.byline',
      score: { sel: '.voters', re: /(-?\d+)/, k: 20 },
      comments: { sel: '.comments_label a', re: /(\d+)\scomments?/, k: 5 },
    });
  },
};

export default lobsters;
