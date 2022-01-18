import { Site } from './types';

const twitter: Site = {
  domain: 'twitter.com',
  onLoad: () => {
    api.mapkey(
      ',n',
      'Nitter',
      () =>
        (window.location.href = window.location.href.replace(
          'twitter.com',
          'nitter.net'
        )),
      { domain: /twitter\.com/ }
    );
    api.mapkey(
      ',t',
      'Threadreader',
      () =>
        (window.location.href = `https://threadreaderapp.com/search?q=${window.location.href}`),
      { domain: /twitter\.com/ }
    );
  },
};

export default twitter;
