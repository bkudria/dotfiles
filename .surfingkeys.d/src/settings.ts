declare var settings: any;
declare var Hints: any;

export const setSettings = () => {
  settings.tabsThreshold = 0;
  settings.scrollStepSize = 100;
  settings.focusFirstCandidate = true;
  settings.hintAlign = 'left';

  Hints.characters = 'hynumi,opl:fgrtvbedcxwqaz';
  Hints.style(`
    font-size: 6pt;
    color: #3c3836;
    font-family: sans-serif;
    background: #fbf1c7;
  `);
};
