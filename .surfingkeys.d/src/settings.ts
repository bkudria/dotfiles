declare var settings: any;

export const setSettings = () => {
  settings.tabsThreshold = 0;
  settings.scrollStepSize = 125;
  settings.focusFirstCandidate = true;
  settings.hintAlign = 'left';

  api.unmap('h');
  api.unmap('l');
  api.unmap('0');

  api.Hints.setCharacters('umyhni8769olp0rvtgbc345wsxqaz12');
  api.Hints.style(`
    font-size: 6pt;
    color: #3c3836;
    font-family: "Menlo";
    background: #fbf1c7;
  `);
};
