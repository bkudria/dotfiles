import { removeSticky } from './utils';

export const applyMappings = () => {
  // Unmap proxy stuff
  unmap('cp');
  unmap(';cp');
  unmap(';ap');

  map('<Backspace>', 'S');
  map(';u', ';U');
  map('h', 'E');
  map('l', 'R');

  map('g-', 'g0');

  map('\\ttn', 'k', /./, 'Scroll up [TTN]');
  map('\\tts', 'j', /./, 'Scroll down [TTS]');
  map('\\tte', 'x', /./, 'Close Tab [TTE]');
  mapkey('\\ttw', 'Scroll page down', () => Normal.scroll('fullPageDown'));

  mapkey('--', 'Remove Sticky', removeSticky);
};
