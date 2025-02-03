import { setSettings } from './settings';
import { applyTheme } from './theme';
import { applyMappings } from './mappings';
import { applyGlobalSiteSettings, applyCurrentSiteSettings } from './sites';

declare global {
  var document: Document;
  var getElements: (selector: string) => HTMLElement[];
  var api: any;
}

setSettings();
applyTheme();
applyMappings();
applyGlobalSiteSettings();
applyCurrentSiteSettings();
