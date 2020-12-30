module.exports = (config) => {
  const version = '1.78.6';
  const shortVersion = version.slice(0, version.lastIndexOf('.')); // '1.23.4' => '1.23'

  const ui5Urls = {
    latest: 'https://sapui5.hana.ondemand.com',
    public: `https://sapui5.hana.ondemand.com/${version}`,
    internal: `http://vesapui5.dhcp.wdf.sap.corp:8080/sapui5-dist-${shortVersion}`,
    nightly: 'https://sapui5nightly.int.sap.hana.ondemand.com',
    nexus: `http://nexus.wdf.sap.corp:8081/nexus/content/unzip/build.snapshots.unzip/com/sap/ui5/dist/sapui5-sdk-dist/${version}/sapui5-sdk-dist-${version}-opt-static.zip-unzip`,
    local: 'http://localhost:5000/sapui5',
  };

  const ui5Url = process.env.UI5_URL || ui5Urls.public;
  console.log('UI5 URL: ', ui5Url);

  config.set({
    frameworks: ['ui5'],

    ui5: {
      url: ui5Url,
      testpage: '/base/test-resources/testsuite.qunit.html',
    },

    proxies: {
      '/base/resources': '/base/webapp/resources',
      '/base/test-resources': '/base/webapp/test',
    },

    browsers: ['Chrome'],

    browserDisconnectTimeout: 60000,

    browserNoActivityTimeout: 120000,

    logLevel: config.LOG_INFO,
  });
};
