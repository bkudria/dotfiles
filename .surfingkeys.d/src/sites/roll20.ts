import { Site } from './types';

const roll20: Site = {
  domain: 'app.roll20.net',
  onLoad: () => {
    api.unmap('`', /app.roll20.net/);
    api.mapkey(
      '!rMicrophone',
      'Roll20 Toggle Audio',
      () =>
        (document?.querySelector(
          `*[id="player_-M2V74cjR6ybxSEet2kQ"] .av-controls .mute-audio`
        ) as HTMLElement).click(),
      { domain: /roll20\.net/ }
    );
    api.mapkey(
      '!rPower',
      'Roll20 Toggle Video',
      () =>
        (document?.querySelector(
          `*[id="player_-M2V74cjR6ybxSEet2kQ"] .av-controls .pause-video`
        ) as HTMLElement).click(),
      { domain: /roll20\.net/ }
    );
  },
};

export default roll20;
