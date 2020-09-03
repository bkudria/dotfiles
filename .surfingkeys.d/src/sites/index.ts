import { chars } from '../utils';

import { Site } from './types';

import amazon from './amazon';
import duckduckgo from './duckduckgo';
import gmail from './gmail';
import google from './google';
import hackernews from './hackernews';
import imgur from './imgur';
import lobsters from './lobsters';
import roll20 from './roll20';
import wikipedia from './wikipedia';

const sites: Site[] = [
  amazon,
  duckduckgo,
  gmail,
  google,
  hackernews,
  imgur,
  lobsters,
  roll20,
  wikipedia,
];

const applyGlobalSiteSettings = () => {
  chars`bdghwyse`.forEach(searchAlias => {
    removeSearchAliasX(searchAlias, 's');
    removeSearchAliasX(searchAlias, 'o');
  });

  sites.forEach(site => {
    site.engines?.forEach(engine => {
      addSearchAliasX(
        engine.alias,
        engine.name,
        engine.search,
        's',
        engine.completion,
        engine.callback
      );

      mapkey(
        `o${engine.single || engine.alias}`,
        `#8Search ${engine.name}`,
        () => Front.openOmnibar({ type: 'SearchEngine', extra: engine.alias })
      );
    });
  });
};

const applyCurrentSiteSettings = () => {
  sites.forEach(site => {
    if (site?.domain === window.location.hostname) {
      site.mappings?.forEach(mapping => {
        if (window.location.pathname.slice(1).match(mapping.path)) {
          mapkey(mapping.keys, mapping.description, mapping.fn);
        }
      });

      site.onLoad && site.onLoad();
    }
  });
};

export { applyGlobalSiteSettings, applyCurrentSiteSettings };
