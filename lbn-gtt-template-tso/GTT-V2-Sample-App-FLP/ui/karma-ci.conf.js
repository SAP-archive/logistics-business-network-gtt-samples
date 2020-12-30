module.exports = (config) => {
  require('./karma.conf')(config);

  config.set({
    preprocessors: {
      'webapp/Component.js': ['coverage'],
      'webapp/!(localService|test)/**/*.js': ['coverage'],
    },

    reporters: ['progress', 'coverage', 'junit'],

    browsers: ['ChromeHeadlessNoSandbox'],

    customLaunchers: {
      ChromeHeadlessNoSandbox: {
        base: 'ChromeHeadless',
        // We must disable the Chrome sandbox when running Chrome inside Docker (Chrome's sandbox needs
        // more permissions than Docker allows by default)
        flags: ['--no-sandbox'],
      },
    },

    junitReporter: {
      outputDir: 'target/surefire-reports',
      // outputFile: 'TEST-com.sap.ui5.selenium.qunit.QUnitTest.xml',
      outputFile: 'unit.allTests.xml',
      suite: '',
      useBrowserName: false,
    },

    coverageReporter: {
      includeAllSources: true,
      dir: 'coverage',
      reporters: [
        {
          type: 'html',
          subdir: 'html',
        },
        {
          type: 'text',
        },
        {
          type: 'text-summary',
        },
        {
          type: 'cobertura',
          subdir: '.',
        },
        {
          type: 'lcovonly',
          subdir: '.',
        },
      ],
      // check: {
      //   global: {
      //     statements: 70,
      //     branches: 0,
      //     functions: 0,
      //     lines: 0
      //   }
      // }
    },

    failOnEmptyTestSuite: false,

    singleRun: true,
  });
};
