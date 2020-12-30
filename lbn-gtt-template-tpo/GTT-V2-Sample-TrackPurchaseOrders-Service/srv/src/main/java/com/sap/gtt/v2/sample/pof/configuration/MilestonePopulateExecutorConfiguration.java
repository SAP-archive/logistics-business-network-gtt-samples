package com.sap.gtt.v2.sample.pof.configuration;

import com.sap.gtt.v2.sample.pof.constant.FulfillmentProcessMilestoneEnum;
import com.sap.gtt.v2.sample.pof.rest.service.fulfillmentProcessFlow.MilestoneCommand;
import com.sap.gtt.v2.sample.pof.rest.service.fulfillmentProcessFlow.MilestonePopulateCommand;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.lang.String.format;

@Configuration
@ComponentScan("com.sap.gtt.v2.sample.pof.rest.service.fulfillmentProcessFlow")
public class MilestonePopulateExecutorConfiguration {

    @Bean
    public Map<FulfillmentProcessMilestoneEnum, MilestonePopulateCommand> commandByMilestoneEnum(List<MilestonePopulateCommand> commands) {
        Map<FulfillmentProcessMilestoneEnum, MilestonePopulateCommand> result = new HashMap<>();
        for (MilestonePopulateCommand command : commands) {
            Class<?> clazz = command.getClass();
            if (!clazz.isAnnotationPresent(MilestoneCommand.class)) {
                throw new IllegalStateException(
                        format("Fulfillment Milestone command processor should be populated with MilestoneCommand annotation for command: %s",
                                clazz.getName()));
            }
            MilestoneCommand annotation = clazz.getAnnotation(MilestoneCommand.class);
            result.put(annotation.value(), command);
        }
        return result;
    }
}
