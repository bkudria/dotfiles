import { removeSticky, scrollMostPage } from './utils';

export const applyMappings = () => {
  // Unmap proxy stuff
  api.unmap('cp');
  api.unmap(';cp');
  api.unmap(';ap');

  api.unmap('D');

  api.iunmap(':'); // No emojis

  api.map('<Backspace>', 'S');
  api.map(';u', ';U');
  api.map('h', 'E');
  api.map('l', 'R');

  api.map('!rUp', 'k', /./, 'Scroll up');
  api.map('!rDown', 'j', /./, 'Scroll down');
  api.map('!rRight', 'x', /./, 'Close Tab');
  // api.map('!rLeft', 'd', /./, 'Scroll page down');
  api.mapkey('!rLeft', 'Scroll page down', scrollMostPage);
  api.mapkey('<Space>', 'Scroll page down', scrollMostPage);

  api.mapkey('__', 'Remove Sticky', removeSticky);
};
