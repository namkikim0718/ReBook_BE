package com.be.rebook.common.restclients;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestClient;
import org.springframework.web.client.support.RestClientAdapter;
import org.springframework.web.service.invoker.HttpServiceProxyFactory;

import com.be.rebook.common.eureka.strategy.InstanceSelectionStrategy;
import com.be.rebook.common.eureka.strategy.impl.RoundRobinStrategy;

import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Component
public class RestClientFactory {

    private final DiscoveryClient discoveryClient;
    private static final Logger restclientfactoryLogger = LoggerFactory.getLogger(RestClientFactory.class);

    /**
     * 인스턴스 목록 중에서 선택된 인스턴스를 이용하여 AuthServiceRestClient 인터페이스를 구현한 클라이언트를 생성한다.
     * 
     * @param instanceList              인스턴스 목록
     * @param instanceSelectionStrategy 인스턴스 선택 전략 (기본값: RoundRobinStrategy)
     * @return AuthServiceRestClient 인터페이스를 구현한 클라이언트
     */
    public AuthServiceRestClient createAuthServiceRestClient(
            InstanceSelectionStrategy instanceSelectionStrategy) {
        ServiceInstance instance = findInstance("rebook-auth", instanceSelectionStrategy);

        restclientfactoryLogger.info("eureka auth URI : {}", instance.getUri().toString());
        RestClient restClient = RestClient.create(instance.getUri().toString());
        RestClientAdapter adapter = RestClientAdapter.create(restClient);
        HttpServiceProxyFactory factory = HttpServiceProxyFactory.builderFor(adapter).build();

        return factory.createClient(AuthServiceRestClient.class);
    }

    private ServiceInstance findInstance(String serviceId,
            InstanceSelectionStrategy instanceSelectionStrategy) {
        restclientfactoryLogger.info("eureka auth serviceID : {}", serviceId);
        List<ServiceInstance> instanceList = discoveryClient.getInstances(serviceId);
        instanceSelectionStrategy = instanceSelectionStrategy == null ? new RoundRobinStrategy()
                : instanceSelectionStrategy;

        return instanceSelectionStrategy.selectInstance(instanceList);
    }
}
