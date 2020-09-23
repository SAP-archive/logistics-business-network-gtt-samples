const Koa = require('koa');
const compress = require('koa-compress');
const conditional = require('koa-conditional-get');
const etag = require('koa-etag');
const serve = require('koa-static');
const mount = require('koa-mount');
const path = require('path');
const dotenv = require('dotenv');

if (process.env.NODE_ENV !== 'production') {
  dotenv.config();
}

const paths = {
  webapp: '/',
  sapui5: '/sapui5',
  vendor: '/vendor',
};

const dir = {
  webapp: 'webapp',
  dist: 'dist',
  vendor: 'webapp/vendor',
};

const app = new Koa();

// compress
app.use(
  compress({
    // threshold: 2048,
  })
);

// use it upstream from etag so that they are present
app.use(conditional());

// etag
app.use(etag());

// cache
// max-age: koa-static in milliseconds, http header in seconds
const ONE_SECOND = 1000;
const ONE_MINUTE = ONE_SECOND * 60;
const ONE_HOUR = ONE_MINUTE * 60;
const ONE_DAY = ONE_HOUR * 24;
const ONE_WEEK = ONE_DAY * 7;
const ONE_MONTH = ONE_DAY * 30;
const ONE_YEAR = ONE_DAY * 365;

// SAPUI5
if (process.env.UI5_DIR) {
  dir.sapui5 = path.resolve(process.env.UI5_DIR);
  app.use(
    mount(
      paths.sapui5,
      serve(dir.sapui5, {
        maxage: ONE_MONTH,
      })
    )
  );
}

// Vendor
app.use(
  mount(
    paths.vendor,
    serve(dir.vendor, {
      maxage: ONE_WEEK,
    })
  )
);

// static
const staticDir = process.env.NODE_ENV === 'production' ? dir.dist : dir.webapp;
app.use(mount(paths.webapp, serve(staticDir)));

const port = process.env.PORT || 5000;
app.listen(port);

// eslint-disable-next-line no-console
console.log(`Server is running on port ${port}.`);
