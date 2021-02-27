sap.ui.define(["sap/base/util/deepEqual"], function (deepEqual) {
  "use strict";

  /**
   * Enable state management in UI5 object.
   *
   * For each provided state, a setter method will be created, likes:
   * `{ loading: false }` -> `setLoading(value)`.
   *
   * If a generated setter is called with a value, it will do a simple check on its type,
   * run the validator and finally check whether it is equal to the previous one.
   * If a valid new value is provided, the state will be updated and
   * the corresponding side-effect will be executed if it exist.
   *
   * The hook's name of side-effect is generated from state's name, for example:
   * `loading` -> `onLoadingChange(changes)`, where `changes` is an object containing
   * the previous value and the current value, e.g., `{ previous: false, current: true }`.
   *
   * In addition, you can customize the validators by using the _Validator Hooks_,
   * the naming rule will be: `{ count: 0 }` -> `validateCount(nextValue, callback)`.
   * If you call the `callback` with an error message, it will throw an `Error`.
   *
   * This function will also create a method to reset all states to their initial values,
   * which named `resetState()`.
   *
   * @param {Record<string, unknown>} state State with initial values
   * @param {*} thisArg Context
   *
   * @example
   * state: {
   *   loading: false,
   *   count: 0,
   * },
   * onInit: function () {
   *   registerState(this.state, this);
   *   this.setLoading(true);
   * },
   * onLoadingChange: function (changes) {
   *   console.log(changes);  // { previous: false, current: true }
   * },
   * validateCount: function (nextValue, callback) {
   *   if (nextValue < 0) {
   *     return callback("'count' cannot be negative, please check");
   *   }
   *   return callback();  // Default behavior
   * },
   * reset: function () {
   *   this.resetState();  // { loading: false, count: 0 }
   *   // ...
   * },
   */
  var registerState = function (state, thisArg) {
    if (!thisArg.state) {
      thisArg.state = state;
    }

    Object.defineProperty(thisArg, "initialState", {
      value: Object.assign({}, state),
      enumerable: true,
    });

    Object.keys(state).forEach(function (stateName) {
      var capitalizedStateName = stateName.replace(/^\S/, function (initial) {
        return initial.toUpperCase();
      });
      var setterName = "set" + capitalizedStateName;
      var validatorName = "validate" + capitalizedStateName;
      var sideEffectName = "on" + capitalizedStateName + "Change";

      Object.defineProperty(thisArg, setterName, {
        /**
         * Setter for certain state
         *
         * @param {*} value New value
         */
        value: function (value) {
          if (typeof value !== typeof state[stateName]) {
            throw new TypeError("Type '" + typeof value + "' is not assignable to type '" + typeof state[stateName] + "'");
          }

          thisArg[validatorName](value, function (err) {
            if (err !== undefined) {
              throw new Error(err);
            }
          });

          var current = thisArg.state[stateName];
          if (!deepEqual(value, current)) {
            var previous = current;
            thisArg.state[stateName] = value;

            thisArg[sideEffectName]({
              previous: previous,
              current: value,
            });
          }
        },
        enumerable: true,
      });

      if (typeof thisArg[validatorName] !== "function") {
        Object.defineProperty(thisArg, validatorName, {
          /**
           * Validator Hook
           *
           * @param {*} nextValue Next value
           * @param {(err?: string) => void} callback Callback
           */
          value: function (nextValue, callback) {
            return callback();
          },
        });
      }

      if (typeof thisArg[sideEffectName] !== "function") {
        Object.defineProperty(thisArg, sideEffectName, {
          /**
           * Side-effect Hook
           *
           * @param {object} changes Changes
           * @param {*} changes.previous Previous value
           * @param {*} changes.current Current value
           */
          value: function (changes) { },
        });
      }
    }, thisArg);

    Object.defineProperty(thisArg, "resetState", {
      /**
       * Reset all states to their initial values
       */
      value: function () {
        Object.assign(thisArg.state, thisArg.initialState);
      },
      enumerable: true,
    });
  };

  return registerState;
});
