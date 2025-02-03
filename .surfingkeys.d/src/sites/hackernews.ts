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

    var scores = document.querySelectorAll(
      'td.subtext span.subline:has(> span.score)'
    );
    scores.forEach(element => {
      var html_element = element as HTMLElement;
      if (html_element.innerText.match(/(\d+)\spoints/)) {
        var match = html_element.innerText.match(/(\d+)\spoints/);
        if (match) {
          var scoreValue = Number(match[1])
            .toString(Math.E)
            .length.toString();
          element.insertAdjacentHTML(
            'beforebegin',
            `<div class="gauge gauge-score gauge-${scoreValue}"></div>`
          );
        }
      }
    });

    var links = document.querySelectorAll('td.subtext span.subline:has(> a)');
    links.forEach(element => {
      var html_element = element as HTMLElement;
      if (html_element.innerText.match(/(\d+)\scomments/)) {
        var match = html_element.innerText.match(/(\d+)\scomments/);
        if (match) {
          var commentsValue = Number(match[1])
            .toString(Math.E)
            .length.toString();
          element.insertAdjacentHTML(
            'beforebegin',
            `<div class="gauge gauge-comment gauge-${commentsValue}"></div>`
          );
        }
      }
    });
  },
};

export default hackernews;
