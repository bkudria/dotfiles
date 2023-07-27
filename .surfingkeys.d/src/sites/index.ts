import { Site } from './types';

import amazon from './amazon';
import disneyplus from './disneyplus';
import duckduckgo from './duckduckgo';
import gmail from './gmail';
import google from './google';
import hackernews from './hackernews';
import hbomax from './hbomax';
import hulu from './hulu';
import imgur from './imgur';
import lobsters from './lobsters';
import netflix from './netflix';
import roll20 from './roll20';
import twitter from './twitter';
import wikipedia from './wikipedia';

declare var api: any;

const sites: Site[] = [
  amazon,
  disneyplus,
  duckduckgo,
  gmail,
  google,
  hackernews,
  hbomax,
  hulu,
  imgur,
  lobsters,
  netflix,
  roll20,
  twitter,
  wikipedia,
];

const applyGlobalSiteSettings = () => {
  // chars('bdghwyse').forEach(searchAlias => {
  //   api.removeSearchAlias(searchAlias, 's');
  //   api.removeSearchAlias(searchAlias, 'o');
  // });

  sites.forEach(site => {
    api.mapkey(
      'ga',
      'Archived Page',
      () =>
        (window.location.href = `https://archive.vn/newest/${window.location.href}`)
    );

    site.engines?.forEach(engine => {
      api.addSearchAlias(
        engine.alias,
        engine.name,
        engine.search,
        's',
        engine.completion,
        engine.callback
      );

      api.mapkey(
        `o${engine.single || engine.alias}`,
        `#8Search ${engine.name}`,
        () => {} //Front.openOmnibar({ type: 'SearchEngine', extra: engine.alias })
      );
    });
  });
};

const applyCurrentSiteSettings = () => {
  sites.forEach(site => {
    if (site?.domain === window.location.hostname) {
      site.mappings?.forEach(mapping => {
        if (window.location.pathname.slice(1).match(mapping.path)) {
          api.mapkey(mapping.keys, mapping.description, mapping.fn);
        }
      });

      site.onLoad && site.onLoad();
    }
  });
};

export { applyGlobalSiteSettings, applyCurrentSiteSettings };
