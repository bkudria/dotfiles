import { darkReaderEnabled, openStoryAndComments } from '../utils';
import { applyLightTheme } from '../theme';

import { Site } from './types';

const hackernews: Site = {
  domain: 'news.ycombinator.com',
  mappings: [
    {
      path: /^(news)?$/,
      keys: 'f',
      description: '#1Open Link and Comments [HN]',
      fn: () =>
        openStoryAndComments({
          story: 'tr.athing',
          link: '.titleline > a',
          comments: '+ tr td.subtext .subline > a:last-of-type',
        }),
    },
  ],
  onLoad: () => darkReaderEnabled() && applyLightTheme(),
};

export default hackernews;
