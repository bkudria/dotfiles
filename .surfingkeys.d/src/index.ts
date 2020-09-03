import { setSettings } from './settings';
import { applyTheme } from './theme';
import { applyMappings } from './mappings';
import { applyGlobalSiteSettings, applyCurrentSiteSettings } from './sites';

declare global {
  var document: Document;
  var getElements: (selector: string) => HTMLElement[];
  var Front: { openOmnibar: (arg: any) => any };
  var Normal: any;
  var Hints: any;
  var addSearchAliasX: any;
  var removeSearchAliasX: any;
  var map: any;
  var mapkey: any;
  var unmap: any;
  var unmapAllExcept: any;
}

setSettings();
applyTheme();
applyMappings();
applyGlobalSiteSettings();
applyCurrentSiteSettings();
