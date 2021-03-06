import { removeSticky } from './utils';

export const applyMappings = () => {
  // Unmap proxy stuff
  unmap('cp');
  unmap(';cp');
  unmap(';ap');

  iunmap(":"); // No emojis

  map('<Backspace>', 'S');
  map(';u', ';U');
  map('h', 'E');
  map('l', 'R');

  map('\\ttn', 'k', /./, 'Scroll up [TTN]');
  map('\\tts', 'j', /./, 'Scroll down [TTS]');
  map('\\tte', 'x', /./, 'Close Tab [TTE]');
  mapkey('\\ttw', 'Scroll page down', () => Normal.scroll('fullPageDown'));

  mapkey('__', 'Remove Sticky', removeSticky);
};
