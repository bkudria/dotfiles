declare var settings: any;
declare var Hints: any;

export const setSettings = () => {
  settings.tabsThreshold = 0;
  settings.scrollStepSize = 150;
  settings.focusFirstCandidate = true;
  settings.hintAlign = 'left';

  Hints.characters = 'umyhni7869olp0rvtgbedc435wsxqaz12';
  Hints.style(`
    font-size: 6pt;
    color: #3c3836;
    font-family: "Menlo";
    background: #fbf1c7;
  `);
};
