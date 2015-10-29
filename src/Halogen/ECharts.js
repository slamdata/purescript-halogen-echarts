// module Halogen.ECharts

exports.memo = {value: {}};
exports.dataset = function(node) {
    return function() {
        return node.dataset;
    };
};
