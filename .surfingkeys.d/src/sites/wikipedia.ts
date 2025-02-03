import { defaultIcon, createSuggestionItem } from '../utils';
import { Site } from './types';

type wikipediaSearchResult = {
  thumbnail?: { source: string };
  description?: string;
  title: string;
  fullurl: string;
};

const wikipediaCallback = (response: { text: string }) =>
  (Object.values(
    JSON.parse(response.text).query.pages
  ) as wikipediaSearchResult[]).map(p => {
    const img = p.thumbnail ? p.thumbnail.source : defaultIcon;
    const desc = p.description ? p.description : '';
    return createSuggestionItem(
      `<div style="padding:5px;display:grid;grid-template-columns:60px 1fr;grid-gap:15px">
       <img style="width:60px" src="${img}" alt="${p.title}">
       <div>
       <div class="title"><strong>${p.title}</strong></div>
       <div class="title">${desc}</div>
       </div>
       </div>`,
      { url: p.fullurl }
    );
  });

const wikipedia: Site = {
  domain: 'en.m.wikipedia.org',
  engines: [
    {
      name: 'Wikipedia',
      alias: 'e',
      search: 'https://en.wikipedia.org/w/index.php?search=',

      completion: `https://en.wikipedia.org/w/api.php?action=query&format=json
                   &generator=prefixsearch&prop=info|pageprops%7Cpageimages%7Cdescription
                   &redirects=&ppprop=displaytitle&piprop=thumbnail&pithumbsize=100&pilimit=6
                   &inprop=url&gpssearch=`.replace(/\s+/g, ''),

      callback: wikipediaCallback,
    },
  ],
};
export default wikipedia;
