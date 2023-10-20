import { Site } from './types';

const disneyplus: Site = {
  domain: 'www.disneyplus.com',
  onLoad: () => api.unmap('<Space>'),
};

export default disneyplus;
