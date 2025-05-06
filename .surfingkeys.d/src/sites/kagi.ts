import { defaultIcon, createSuggestionItem } from '../utils';
import { Site } from './types';

type kagiSearchResult = {
  // thumbnail?: { source: string };
  t: string;
  txt?: string;
  goto: string;
  img?: string;
};

const kagi: Site = {
  domain: 'kagi.com',
  engines: [
    {
      name: 'Kagi',
      alias: 's',
      search: 'https://kagi.com/search?q=',
      completion: 'https://kagi.com/api/autosuggest?q=',
      callback: response =>
        (JSON.parse(response.text) as kagiSearchResult[]).map(r => {
          const u = new URL('https://kagi.com/search');
          u.searchParams.append('q', r.t);
          if (r.goto) {
            u.href = r.goto;
          }
          return createSuggestionItem(
            `
      <div style="padding: 5px; display: grid; grid-template-columns: 32px 1fr; grid-gap: 15px">
        <img style="width: 32px" src="${
          r.img ? new URL(r.img, 'https://kagi.com') : defaultIcon
        }" />
        <div>
          <div class="title"><strong>${r.t}</strong></div>
          <div class="title">${r.txt ?? ''}</div>
        </div>
      </div>
    `,
            { url: u.href }
          );
        }),
    },
  ],
};
export default kagi;
