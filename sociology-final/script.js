function changeGraph() {
    const dropdown = document.getElementById("demographic");
    const image = document.getElementById("demo-choice");
    const selected = dropdown.value;

    if (selected == "asian") {
        image.src = "graphs/asian.png";
    } else if (selected == "black") {
        image.src = "graphs/black.png";
    } else if (selected == "female") {
        image.src = "graphs/female.png";
    } else if (selected == "hispanic") {
        image.src = "graphs/hispanic.png";
    } else if (selected == "male") {
        image.src = "graphs/male.png";
    } else if (selected == "white") {
        image.src = "graphs/white.png";
    } else {
        image.src = "graphs/all.png";
    } 
}