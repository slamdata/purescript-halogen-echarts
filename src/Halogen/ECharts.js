// module Halogen.ECharts

exports.memo = {value: {}};
exports.initialized = {value: false};
exports.dataset = function(node) {
    return function() {
        return node.dataset;
    };
};
