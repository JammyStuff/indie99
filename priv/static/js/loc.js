$(document).ready(function() {
    var location_checkbox = $("#location-checkbox");
    location_checkbox.change(function() {
        if (location_checkbox.is(":checked")) {
            var location_watch_id = navigator.geolocation.watchPosition(function(position) {
                $("#latitude-box").val(position.coords.latitude);
                $("#longitude-box").val(position.coords.longitude);
            });
        } else {
            navigator.geolocation.clearWatch(location_watch_id);
        }
    });
});
