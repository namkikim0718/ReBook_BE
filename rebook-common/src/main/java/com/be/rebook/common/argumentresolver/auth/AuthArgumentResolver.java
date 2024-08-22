package com.be.rebook.common.argumentresolver.auth;

import java.time.LocalDateTime;
import java.util.List;

import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.core.MethodParameter;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.support.WebDataBinderFactory;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.method.support.ModelAndViewContainer;

import com.be.rebook.common.config.BaseResponse;
import com.be.rebook.common.exception.BaseException;
import com.be.rebook.common.exception.ErrorCode;
import com.be.rebook.common.restclients.AuthServiceRestClient;
import com.be.rebook.common.restclients.RestClientFactory;

public class AuthArgumentResolver implements HandlerMethodArgumentResolver {

    private final DiscoveryClient discoveryClient;

    public AuthArgumentResolver(DiscoveryClient discoveryClient) {
        this.discoveryClient = discoveryClient;
    }

    @Override
    public boolean supportsParameter(MethodParameter parameter) {
        return parameter.getParameterAnnotation(Auth.class) != null &&
                parameter.getParameterType().equals(MemberLoginInfo.class);
    }

    @Override
    public Object resolveArgument(MethodParameter parameter, ModelAndViewContainer mavContainer,
            NativeWebRequest webRequest, WebDataBinderFactory binderFactory) throws BaseException {
        try {
            String authorizationHeader = webRequest.getHeader("Authorization");

            List<ServiceInstance> instanceList = discoveryClient.getInstances("rebook-auth");
            AuthServiceRestClient authServiceRestClient = RestClientFactory.createAuthServiceRestClient(instanceList, null);


            // TODO: Unauthorized 해결 안됨 ...
            ResponseEntity<BaseResponse<MemberLoginInfo>> res;
            try {
                res = authServiceRestClient.authenticate(authorizationHeader);
            } catch (Exception e) {
                return null;
            }


            System.out.println(LocalDateTime.now());

            // 응답 상태 코드가 200 OK인지 확인
            if (res.getStatusCode().is2xxSuccessful()) {
                BaseResponse<MemberLoginInfo> baseResponse = res.getBody();

                if (baseResponse != null && baseResponse.getResult() != null) {
                    MemberLoginInfo memberLoginInfo = baseResponse.getResult();
                    System.out.println("AuthArgumentResolver.resolveArgument() memberLoginInfo: " + memberLoginInfo);
                    return memberLoginInfo;
                } else {
                    throw new IllegalArgumentException("응답에서 MemberLoginInfo를 가져올 수 없습니다.");
                }
            } else {
                throw new BaseException(ErrorCode.UNAUTHORIZED); // TODO: 적절한 에러처리 필요
            }
        } catch (Exception e){
            e.printStackTrace();
            throw e;
        }

    }
}
