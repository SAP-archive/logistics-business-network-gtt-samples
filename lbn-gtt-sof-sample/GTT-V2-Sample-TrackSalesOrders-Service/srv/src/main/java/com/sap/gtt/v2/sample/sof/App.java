package com.sap.gtt.v2.sample.sof;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.DefaultUriBuilderFactory;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

@SpringBootApplication
public class App {
	public static void main(String[] args) {
		SpringApplication application = new SpringApplication(App.class);
		application.run(args);

	}

	@Bean
	public RestTemplate restTemplate() {
		RestTemplate restTemplate = new RestTemplate();
		DefaultUriBuilderFactory defaultUriBuilderFactory = new DefaultUriBuilderFactory();
		defaultUriBuilderFactory.setEncodingMode(DefaultUriBuilderFactory.EncodingMode.VALUES_ONLY);
		restTemplate.setUriTemplateHandler(defaultUriBuilderFactory);
		return restTemplate;
	}

	@Bean
	public ExecutorService executorService() {
		return new ThreadPoolExecutor(10, 10, 0L, TimeUnit.MILLISECONDS,
						new LinkedBlockingQueue<Runnable>());
	}
}
