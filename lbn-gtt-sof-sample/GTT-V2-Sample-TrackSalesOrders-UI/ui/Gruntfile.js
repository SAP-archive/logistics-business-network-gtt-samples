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
};
