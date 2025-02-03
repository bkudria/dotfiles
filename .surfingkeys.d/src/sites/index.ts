import { Site } from './types';
import { chars } from '../utils';
import * as sitesImports from './sites';

declare var api: any;

const sites: Site[] = Object.values(sitesImports);

const applyGlobalSiteSettings = () => {
  // Remove default engines
  chars`bdghwyse`.forEach(searchAlias => {
    api.removeSearchAlias(searchAlias, 's');
    api.removeSearchAlias(searchAlias, 'o');
  });

  sites.forEach(site => {
    site.engines?.forEach(engine => {
      api.addSearchAlias(
        engine.alias,
        engine.name,
        engine.search,
        's',
        engine.completion,
        engine.callback
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
