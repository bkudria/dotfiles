import { chars } from '../utils';
import { Site } from './types';

const gmail: Site = {
  domain: 'mail.google.com',
  onLoad: () => unmapAllExcept(chars`f`, /mail\.google\.com/),
};

export default gmail;
