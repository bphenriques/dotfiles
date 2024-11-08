const bat = await Service.import("battery")

export default async function init() {
    bat.connect("notify::percent", ({ percent, charging }) => {
        const low = 30
        if (percent !== low || percent !== low / 2 || !charging)
            return

        Utils.notify({
            summary: `${percent}% Battery Percentage`,
            urgency: "critical",
        })
    })
}