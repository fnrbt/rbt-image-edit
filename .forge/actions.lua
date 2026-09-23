local publish = [[set -euo pipefail; python3 .forge/publish]]

return {
    select = function(ctx)
        if string.match(ctx.ref_name, "^refs/tags/v") then
            return { "publish" }
        end
        return {}
    end,
    publish = function(_)
        return {
            output = "package",
            base = publish,
            packages = {
                -- Restores Rbt dependencies through the sandbox's credentialed gateway.
                read = {
                    { registry = "41021897", ecosystem = "nuget" },
                },
                publish = {
                    { registry = "41021897", ecosystem = "nuget", mode = "explicit", prefix = "Rbt.ImageEdit" },
                },
            },
        }
    end,
}
