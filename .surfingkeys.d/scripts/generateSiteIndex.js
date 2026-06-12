const fs = require('fs');
const path = require('path');

const sitesDir = path.join(__dirname, '../src/sites');
const indexFile = path.join(sitesDir, 'sites.ts');

const generateSiteIndex = () => {
  const files = fs
    .readdirSync(sitesDir)
    .filter(
      file => file.endsWith('.ts') && file !== 'index.ts' && file !== 'sites.ts' && file !== 'types.ts'
    );
  const imports = files
    .map(
      file =>
        `import ${path.basename(file, '.ts')} from './${path.basename(
          file,
          '.ts'
        )}';`
    )
    .join('\n');
  const exports = `export {\n  ${files
    .map(file => path.basename(file, '.ts'))
    .join(',\n  ')},\n};`;

  const content = `${imports}\n\n${exports}\n`;

  fs.writeFileSync(indexFile, content);
  console.log('Site index generated successfully.');
};

generateSiteIndex();
