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

  map('!auxUp', 'k', /./, 'Scroll up');
  map('!auxDown', 'j', /./, 'Scroll down');
  map('!auxRight', 'x', /./, 'Close Tab');
  mapkey('!auxLeft', 'Scroll page down', () => Normal.scroll('fullPageDown'));

  mapkey('__', 'Remove Sticky', removeSticky);
};
