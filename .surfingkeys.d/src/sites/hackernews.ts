import {
  darkReaderEnabled,
  openStoryAndComments,
  renderGauges,
} from '../utils';
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
  engines: [
    {
      name: 'HN',
      alias: 'hn',
      search:
        'https://hn.algolia.com/?dateRange=all&page=0&prefix=true&sort=byPopularity&type=story&query=',
    },
  ],
  onLoad: () => {
    darkReaderEnabled() && applyLightTheme();

    renderGauges({
      rows: 'td.subtext',
      anchor: 'span.subline',
      score: { re: /(\d+)\spoints/, cap: 500 },
      comments: { re: /(\d+)\scomments/, cap: 300 },
    });
  },
};

export default hackernews;
