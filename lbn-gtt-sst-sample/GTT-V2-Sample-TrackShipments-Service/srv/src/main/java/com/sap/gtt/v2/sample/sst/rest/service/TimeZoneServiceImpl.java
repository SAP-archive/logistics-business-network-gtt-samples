package com.sap.gtt.v2.sample.sst.rest.service;

import com.sap.gtt.v2.sample.sst.rest.model.TimeZone;
import com.sap.gtt.v2.sample.sst.rest.model.TimeZoneList;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.TextStyle;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Service;

/**
 * @author Min Li
 */
@Service
public class TimeZoneServiceImpl implements TimeZoneService {

    @Override
    public TimeZoneList getAllTimeZones() {
        TimeZoneList list = new TimeZoneList();

        LocalDateTime local = LocalDateTime.now();
        Locale locale = LocaleContextHolder.getLocale();

        List<TimeZone> timeZones = ZoneId.getAvailableZoneIds().stream()
                .sorted((prev, next) -> local.atZone(ZoneId.of(next)).compareTo(local.atZone(ZoneId.of(prev))))
                .map(zoneId -> {
                    ZoneId zone = ZoneId.of(zoneId);
                    ZonedDateTime zonedDateTime = local.atZone(zone);
                    String offset = zonedDateTime.getOffset().getId();

                    TimeZone timeZone = new TimeZone();
                    String utc = "UTC" + offset.replace("Z", "+00:00");
                    timeZone.setTimeZoneCode(zoneId);
                    timeZone.setDescription(String.format("%s (%s)",
                            zone.getDisplayName(TextStyle.FULL_STANDALONE, locale), utc));
                    timeZone.setOffset(offset);

                    return timeZone;
                }).collect(Collectors.toList());

        list.setItems(timeZones);
        return list;
    }
}
