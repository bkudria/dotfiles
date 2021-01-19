import { openStoryAndComments } from '../utils';
import { Site } from './types';

const hackernews: Site = {
  domain: 'news.ycombinator.com',
  mappings: [
    {
      path: /^$/,
      keys: 'f',
      description: '#1Open Link and Comments [Lobsters]',
      fn: () =>
        openStoryAndComments({
          story: 'tr.athing',
          link: 'a.storylink',
          comments: '+ tr td.subtext > a:last-of-type',
        }),
    },
  ],
};

export default hackernews;
