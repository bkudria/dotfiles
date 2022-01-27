import { Site } from './types';

const hbomax: Site = {
  domain: 'play.hbomax.com',
  onLoad: () => api.unmap('<Space>'),
};

export default hbomax;
