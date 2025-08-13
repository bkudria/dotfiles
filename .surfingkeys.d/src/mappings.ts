import { removeSticky, scrollBy, stayHere } from './utils';

export const applyMappings = () => {
  // Unmap proxy stuff
  api.unmap('cp');
  api.unmap(';cp');
  api.unmap(';ap');

  api.unmap('D');

  api.iunmap(':'); // No emojis

  api.map('ge', ';U');
  api.map('gE', ';u');

  api.map(',db', ';db');

  api.map('<Backspace>', 'S');
  api.map('h', 'E');
  api.map('l', 'R');
  api.map('`', 'x');
  // api.map('oo', '<Ctrl-6>'); // Switch to last used tab

  api.map('))', ']]');

  api.map('!rUp', 'k', /./, 'Scroll up');
  api.map('!rDown', 'j', /./, 'Scroll down');
  api.map('!rRight', 'x', /./, 'Close Tab');

  api.mapkey('e', 'Scroll ~half-page up', () => scrollBy(-0.7));
  api.mapkey('d', 'Scroll ~half-page down', () => scrollBy(0.7));

  api.mapkey('!rLeft', 'Scroll page down', () => scrollBy(0.9));
  api.mapkey('<Space>', 'Scroll page down!', () => scrollBy(0.9));

  api.mapkey('<PageUp>', 'Scroll page up!', () => scrollBy(-0.9));
  api.mapkey('<PageDown>', 'Scroll page down!', () => scrollBy(0.9));

  api.mapkey('__', 'Remove Sticky', () =>
    removeSticky({ root: document, nice: false })
  );

  api.mapkey(
    'ga',
    'Archived Page',
    () =>
      (window.location.href = `https://archive.vn/newest/${window.location.href}`)
  );

  api.mapkey('g.', 'Stay Here', () => stayHere());
};
