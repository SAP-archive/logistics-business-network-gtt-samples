const path = require('path');
const { CLIEngine } = require('eslint');

module.exports = (grunt) => {
  'use strict';

  // build
  grunt.loadNpmTasks('@sap/grunt-sapui5-bestpractice-build');
  grunt.config.merge({
    compatVersion: 'edge',
    deploy_mode: 'html_repo',
  });
  grunt.registerTask('default', [
    'clean:dist',
    'clean:target',
    'copy:copyLibsToDist',
    'clean:vendor',
    'eslint-report',
    'lint',
    'build',
    'copy:copyLibsToApp',
  ]);

  // clean:target
  grunt.config.merge({
    clean: {
      target: ['target'],
      vendor: ['<%= dir.appFolder %>/vendor'],
    },
  });

  grunt.config.merge({
    copy: {
      copyLibsToDist: {
        files: [
          {
            expand: true,
            cwd: '<%= dir.appFolder %>/vendor',
            src: ['**/*'],
            dest: '<%= dir.dist %>/vendor',
          },
        ],
      },
      copyLibsToApp: {
        files: [
          {
            expand: true,
            cwd: '<%= dir.dist %>/vendor',
            src: ['**/*'],
            dest: '<%= dir.appFolder %>/vendor',
          },
        ],
      },
    },
  });

  // sonar scan
  const sonarGroupId = 'com.sap.gtt.v2';
  const sonarArtifactId = process.env.npm_package_name;
  const sonarBranch = process.env.SONAR_BRANCH || 'master';
  const sonarProjectKey = `${sonarGroupId}:${sonarArtifactId}:${sonarBranch}`;

  grunt.loadNpmTasks('grunt-sonar-runner');
  grunt.config.set('sonarRunner', {
    analysis: {
      options: {
        debug: true,
        projectHome: './',
        sonar: {
          host: {
            url: 'https://sonarci.wdf.sap.corp:8443/sonar',
          },
          login: '4716347bf1abc1e3386aaeb0542ceef83ebfc912', // I068400 <roger.xu@sap.com>
          projectKey: sonarProjectKey,
          projectName: process.env.npm_package_description,
          projectVersion: process.env.npm_package_version,
          sources: 'webapp',
          tests: 'webapp/test',
          exclusions: 'webapp/test/**,webapp/localService/**',
          eslint: {
            reportPaths: 'target/eslint.json',
          },
          javascript: {
            eslint: {
              reportPath: 'target/jslint-result.xml',
              overrideSeverity: false,
            },
            qunit: {
              reportPath: 'target/surefire-reports',
            },
            lcov: {
              reportPaths: 'coverage/lcov.info',
            },
          },
          coverage: {
            exclusions: 'webapp/localService/**',
          },
        },
      },
    },
  });

  // tasks
  grunt.registerTask('eslint-report', 'ESLint Report', () => {
    const customRulePath = path.relative(
      '',
      path.join(
        path.dirname(require.resolve('@sap/di.code-validation.js')),
        'defaultConfig',
        'fioriCustomRules',
        '.eslintrules'
      )
    );
    console.log('custom rule path:', customRulePath);

    const cli = new CLIEngine({
      rulePaths: [customRulePath],
    });

    const report = cli.executeOnFiles(['webapp']);

    // output reports
    const outputs = {
      console(output) {
        return console.log(output);
      },
      file(output, format) {
        if (!format.path) {
          console.error(
            `a 'path' prop is required for this format (${format.name}), please specify and run again`
          );
          return;
        }

        try {
          return grunt.file.write(format.path, output);
        } catch (err) {
          console.error(`Could not write file for eslint format: ${format.name} - ${err.message}`);
        }
      },
    };

    const formats = [
      {
        name: 'codeframe',
        output: 'console',
      },
      {
        name: 'stylish',
        output: 'console',
      },
      {
        name: 'json',
        output: 'file',
        path: 'target/eslint.json',
      },
      {
        name: 'checkstyle',
        output: 'file',
        path: 'target/checkstyle-result.xml',
      },
      {
        name: 'jslint-xml',
        output: 'file',
        path: 'target/jslint-result.xml',
      },
    ];

    formats.forEach((format) => {
      const formatter = cli.getFormatter(format.name);
      if (formatter) {
        const outputMethod = outputs[format.output] || outputs.console;
        outputMethod(formatter(report.results), format, report);
      } else {
        console.error(`could not find formatter with name ${format.name}`);
      }
    });

    if (report.errorCount > 0) {
      return false;
    }

    return true;
  });

  grunt.registerTask('eslint', ['clean:target', 'eslint-report']);

  grunt.registerTask('sonar', 'SonarQube Scan', () => {
    if (process.env.SONAR_BRANCH) {
      grunt.task.run('sonarRunner:analysis');
    }
  });
};
