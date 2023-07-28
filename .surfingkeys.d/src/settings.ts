declare var settings: any;

export const setSettings = () => {
  settings.tabsThreshold = 0;
  settings.scrollStepSize = 75;
  settings.focusFirstCandidate = true;
  settings.hintAlign = 'left';

  api.unmap('h');
  api.unmap('l');
  api.unmap('0');

  api.Hints.setCharacters(
    'uyi cre mnh vbgt 6789 12345 olp wsxqaz'.replace(/\s/g, '')
  );
};
