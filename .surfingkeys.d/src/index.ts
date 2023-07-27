import { setSettings } from './settings';
import { applyTheme } from './theme';
import { applyMappings } from './mappings';
import { applyGlobalSiteSettings, applyCurrentSiteSettings } from './sites';
// import { removeSticky } from './utils';

declare global {
  var document: Document;
  var getElements: (selector: string) => HTMLElement[];
  var api: any;
}

// window.self.addEventListener('load', () =>
//   removeSticky({ root: document, nice: true })
// );

setSettings();
applyTheme();
applyMappings();
applyGlobalSiteSettings();
applyCurrentSiteSettings();
